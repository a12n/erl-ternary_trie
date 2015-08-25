.PHONY: all clean distclean shell test

ERL ?= erl
ERL_FLAGS ?= -smp -pa ebin/
REBAR ?= ./rebar

all: $(REBAR)
	$(REBAR) compile

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	rm -rf .eunit .rebar ebin rebar

rebar:
	wget "https://github.com/rebar/rebar/releases/download/2.5.1/rebar" -O $@
	chmod +x $@

shell:
	$(ERL) $(ERL_FLAGS) -s ternary_trie new

test: $(REBAR)
	$(REBAR) eunit