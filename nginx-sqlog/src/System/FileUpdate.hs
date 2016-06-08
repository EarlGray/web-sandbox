module System.FileUpdate (
  FileUpdate, UpdateChan,

  subscribe,
  receive, tryReceive,
  watch, watchUntil, watchWithResult
) where

import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import Data.Time

import Control.Monad (when, forever, unless)
import Control.Concurrent (threadDelay, yield, forkIO)
import Control.Concurrent.STM as STM

import System.IO as IO
import System.FilePath (splitFileName, takeFileName)
import System.Directory (doesFileExist)
import System.FSNotify as FS

import Text.Printf (hPrintf)
import qualified Text.Printf as Pf


{-- Administrivia --}
perror :: Pf.HPrintfType r => String -> r
perror = hPrintf IO.stderr
sleepSeconds n = threadDelay $ 1000 * 1000 * n

-- hms = formatTime defaultTimeLocale "%T"

{-- Interface --}
type FileUpdate = BL.ByteString
type UpdateChan = TChan FileUpdate

receive :: UpdateChan -> IO FileUpdate
receive = chanRecv

tryReceive :: UpdateChan -> IO (Maybe FileUpdate)
tryReceive ch = atomically $ STM.tryReadTChan ch

subscribe :: FilePath -> IO UpdateChan
subscribe fpath = do
  evch <- chanNew
  updch <- chanNew
  forkIO $ tailf evch fpath
  forkIO $ watcher evch updch
  return updch

watchWithResult :: FilePath -> (FileUpdate -> IO (Maybe r)) -> IO r
watchWithResult fpath action = do
  ch <- subscribe fpath
  go ch action
  where
    go ch action = do
      upd <- receive ch
      ret <- action upd  
      case ret of
        Nothing -> go ch action
        Just val -> return val 

watchUntil :: FilePath -> (FileUpdate -> IO Bool) -> IO ()
watchUntil fpath action = do
  ch <- subscribe fpath
  go ch action
  where
    go ch action = do
      upd <- receive ch
      ret <- action upd
      unless ret $ go ch action

watch :: FilePath -> (FileUpdate -> IO ()) -> IO ()
watch fpath action = do
  ch <- subscribe fpath
  go ch action
  where
    go ch action = do
      upd <- receive ch
      ret <- action upd
      go ch action

{--  --}
chanSend :: STM.TChan a -> a -> IO ()
chanSend ch msg = atomically $ STM.writeTChan ch msg

chanRecv :: STM.TChan a -> IO a
chanRecv ch = atomically $ STM.readTChan ch

chanNew :: IO (STM.TChan a)
chanNew = atomically $ newTChan

tailf :: TChan FS.Event -> FilePath -> IO ()
tailf ch fpath = FS.withManager $ \fswatch -> do
    let (dir, fname) = splitFileName fpath

    t <- getCurrentTime
    chanSend ch $ FS.Added fpath $ t

    stop <- FS.watchDir fswatch dir (const True) $ \ev ->
        when (takeFileName (FS.eventPath ev) == fname) $
            chanSend ch ev

    forever $ sleepSeconds 10

watcher :: TChan FS.Event -> TChan FileUpdate -> IO ()
watcher inp out = go inp out M.empty
  where
    readnews fpath offset = do
      exists <- doesFileExist fpath
      case exists of
        True -> IO.withBinaryFile fpath IO.ReadMode $ \h -> do
                    IO.hSeek h IO.SeekFromEnd 0
                    flen <- IO.hTell h
                    let to_read' = fromInteger (flen - offset)
                        (off, to_read) = if to_read' >= 0
                                         then (offset, to_read')
                                         else (0, fromInteger flen)
                    IO.hSeek h IO.AbsoluteSeek off
                    buf <- BL.hGet h to_read
                    return (flen, buf)
        False -> return (0, BL.empty)

    update fpath state = do
      let fname = takeFileName fpath
          offset = fromMaybe 0 $ M.lookup fname state
      (flen, buf) <- readnews fpath offset
      let state' = M.alter (const $ Just flen) fname state
      return (buf, state')

    handle event state = do
      let fpath = FS.eventPath event
          fname = takeFileName fpath
      case event of
        FS.Added _ t ->
          -- perror "[%s] Added\t%s\n" (hms t) fname
          update fpath state
        FS.Modified _ t ->
          -- perror "[%s] Modified\t%s\n" (hms t) fname
          update fpath state
        FS.Removed _ t ->
          -- perror "[%s] Removed\t%s\n" (hms t) fname
          return (BL.empty, M.delete fname state)
        
    go inp out state = do
      ev <- chanRecv inp
      (news, state') <- handle ev state
      chanSend out news
      go inp out state'

