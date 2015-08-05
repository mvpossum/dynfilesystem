ERLC := erlc
ERL := erl
TERMINAL := xterm
HEADER_FILES := $(wildcard *.hrl)
ERL_FILES := $(wildcard *.erl)
BEAM_FILES := $(ERL_FILES:.erl=.beam)

sadfs: $(BEAM_FILES)

%.beam: %.erl $(HEADER_FILES)
	$(ERLC) $<
	
ifndef port
port := 0
endif
run: sadfs
ifndef cant
	@$(ERL) -run worker start $(port) $(folder)
else
	@for i in {1..$(cant)}; do ($(TERMINAL) -e "$(ERL) -run worker start 0 server.$$i || sleep 1d"&); done
endif

kill:
	killall xterm
	
.PHONY: clean

clean:
	rm -rf *.beam erl_crash.dump clients/*.pyc clients/__pycache__
