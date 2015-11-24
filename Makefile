.PHONY: all deps compile test eunit inttest clean

RETEST=${PWD}/deps/retest/retest
LOG_LEVEL?=debug
RT_TARGETS?=inttest

all: deps compile test dialyze

deps:
	@REBAR_EXTRA_DEPS=1 rebar get-deps u-d
	@(cd deps/retest && rebar compile escriptize)

compile:
	rebar co

test: eunit inttest

eunit:
	rebar eunit skip_deps=true

inttest:
	@${RETEST} -l ${LOG_LEVEL} ${RT_TARGETS}

dialyze: .rebar/*.plt
	rebar dialyze

.rebar/*.plt:
	rebar build-plt

clean:
	rebar clean
	-rm -rf .rebar
