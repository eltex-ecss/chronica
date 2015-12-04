.PHONY: all deps compile test eunit inttest clean travis

REBAR?=${PWD}/rebar
RELX?=${PWD}/relx
RETEST=${PWD}/deps/retest/retest
LOG_LEVEL?=debug
RT_TARGETS?=inttest

all: deps compile test dialyze

deps:
	@REBAR_EXTRA_DEPS=1 rebar get-deps u-d
	@(cd deps/retest && rebar compile escriptize)

compile:
	rebar compile

test: eunit inttest

eunit:
	rebar eunit skip_deps=true

inttest:
	@REBAR=rebar RELX=relx ${RETEST} -l ${LOG_LEVEL} ${RT_TARGETS}

dialyze: .rebar/*.plt
	rebar dialyze

.rebar/*.plt:
	rebar build-plt

travis:
	@REBAR_EXTRA_DEPS=1 $(REBAR) get-deps compile
	@(cd deps/retest && $(REBAR) compile escriptize)
	$(REBAR) eunit skip_deps=true
	@REBAR=$(REBAR) RELX=$(RELX) ${RETEST} -l ${LOG_LEVEL} ${RT_TARGETS}

clean:
	rebar clean
	-rm -rf .rebar