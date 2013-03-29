APP:=erli
REBAR:=./rebar

dep:
	@$(REBAR) get-deps

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

all: dep compile

generate: all
	@$(REBAR) generate
