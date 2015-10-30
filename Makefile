.PHONY: all deps compile eunit clean

all: deps compile eunit dialyze

deps:
	rebar g-d u-d

compile:
	rebar co

eunit:
	rebar eunit skip_deps=true

dialyze: .rebar/*.plt 
	rebar dialyze

.rebar/*.plt:
	rebar build-plt

clean:
	rebar clean
	-rm -rf .rebar
