REBAR=$(shell which rebar || ./rebar)
SYNC_PATH = $(ERL_LIBS)/sync
PROPER_PATH = $(ERL_LIBS)/proper

all: deps compile

compile:
		@$(REBAR) compile

app:
		@$(REBAR) compile skip_deps=true

deps:
		@$(REBAR) get-deps

clean:
		@$(REBAR) clean

distclean: clean
		@$(REBAR) delete-deps

test: app
		@$(REBAR) eunit skip_deps=true
