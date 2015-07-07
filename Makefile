ERLC := erlc
ERL := erl
TERMINAL := xterm
HEADER_FILES := $(wildcard *.hrl)
ERL_FILES := $(wildcard *.erl)
BEAM_FILES := $(ERL_FILES:.erl=.beam)

sadfs: $(BEAM_FILES)

%.beam: %.erl $(HEADER_FILES)
	$(ERLC) $<
	
ifndef cant
    cant:=1
endif

run: sadfs
ifeq ($(cant),1)
	@$(ERL) -run worker start
else
	@for i in {1..$(cant)}; do ($(TERMINAL) -e "$(ERL) -run worker start $$(($$RANDOM%(65535-1024)+1024)) || sleep 1d"&); done
endif

kill:
	killall xterm
	
.PHONY: clean

clean:
	rm -f *.beam erl_crash.dump
