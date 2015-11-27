ERLC := erlc
ERL := erl
NOSE := nosetests2
TERMINAL := xterm
PYTHON := python2
HEADER_FILES := $(wildcard *.hrl)
ERL_FILES := $(wildcard *.erl)
BEAM_FILES := $(ERL_FILES:.erl=.beam)

%.beam: %.erl $(HEADER_FILES)
	$(ERLC) $<
	
cawfs: $(BEAM_FILES)
	
port = 0
run: cawfs
ifndef cant
	@$(ERL) -run main start $(port) $(folder)
else
	@for i in {1..$(cant)}; do ($(TERMINAL) -e "$(ERL) -run main start 0 server.$$i || sleep 1d"&); done
endif

test:
	@$(NOSE) clients/cawtest.py  --processes=4

telnet:
	@$(PYTHON) clients/cawtelnet.py

mount = client
fuse:
	@mkdir -p $(mount)
	@$(PYTHON) clients/cawfuse.py $(mount)

kill:
	killall xterm
	
.PHONY: clean
clean:
	rm -rf *.beam erl_crash.dump clients/*.pyc clients/__pycache__ server* client/
