ERLC := erlc
HEADER_FILES := $(wildcard *.hrl)
ERL_FILES := $(wildcard *.erl)
BEAM_FILES := $(ERL_FILES:.erl=.beam)

tpsoi: $(BEAM_FILES)
%.beam: %.erl $(HEADER_FILES)
	$(ERLC) $<

run: tpsoi
	erl -run worker s

cant: tpsoi
	for i in {1..$(k)}; do (xterm -e "erl -run worker s $$(($$RANDOM%10000+2000)) || sleep 1d"&); done

.PHONY: clean

clean:
	rm -f *.beam erl_crash.dump
