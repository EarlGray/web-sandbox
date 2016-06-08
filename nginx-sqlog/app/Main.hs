module Main where

import qualified Data.ByteString.Lazy as BL

import System.FileUpdate (watchUntil)

import System.IO as IO
import qualified System.Environment as Env

import Text.Printf (hPrintf)


main :: IO ()
main = do
  args <- Env.getArgs
  let filename = args !! 0
  hPrintf IO.stderr "Watching %s...\n" filename
  watchUntil filename $ \upd -> do
    BL.putStr upd
    return $ BL.null upd
  hPutStrLn IO.stderr "Bye"
