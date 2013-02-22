APP:=erli
REBAR:=./rebar

all: deps compile

deps:
	@$(REBAR) get-deps

generate: all
	@$(REBAR) generate

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

eunit:
	@$(REBAR) skip_deps=true eunit

compile:
	@$(REBAR) compile
