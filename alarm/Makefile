.PHONY: run clean

VENV=venv
VIRTUALENV=virtualenv-3.5

run: $(VENV)
	. $(VENV)/bin/activate && python alarm.py

$(VENV):
	$(VIRTUALENV) $(VENV)
	. $(VENV)/bin/activate \
		&& pip install --upgrade pip \
		&& pip install -r requirements.txt

clean:
	rm -rf $(VENV)
