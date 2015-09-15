REBAR=`which rebar || echo ./rebar`
all: compile docs
update: get-deps update-deps
full: clean-all get-deps update-deps compile docs full-tests
ci: get-deps update-deps compile docs tests
prod: get-deps update-deps compile-prod tests docs

get-deps:
	@$(REBAR) get-deps
update-deps:
	@$(REBAR) update-deps
compile:
	@$(REBAR) compile
compile-prod:
	@$(REBAR) compile 
tests:
	@$(REBAR) skip_deps=true eunit
unit-tests:
	@$(REBAR) skip_deps=true eunit
full-tests:
	@$(REBAR) skip_deps=true eunit ct
clean:
	@$(REBAR) skip_deps=true clean
clean-all:
	@$(REBAR) clean
docs:
	@$(REBAR) skip_deps=true doc
