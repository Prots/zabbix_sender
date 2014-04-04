REBAR=`which rebar || echo ./rebar`
DIALYZER = dialyzer

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) skip_deps=true clean

cleanall:
	@$(REBAR) clean

tests: eunit

eunit:
	@$(REBAR) skip_deps=true eunit

check: xref dialyzer

xref:
	@$(REBAR) skip_deps=true xref

dialyzer:
	@$(DIALYZER) -q -n -I include --src src/*.erl -Werror_handling \
		-Wrace_conditions -Wno_return # -Wunmatched_returns -Wunderspecs

docs:
	@$(REBAR) doc
