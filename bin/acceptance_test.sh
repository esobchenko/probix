#!/bin/sh
cd `dirname $0`/..

# because I want to run tests while my server is running
export PROBIX_SERVER_IP="127.0.0.1"
export PROBIX_SERVER_PORT="1234"

## Setting running defaults
export PROBIX_TEST_MODE=1
export ERL_MAX_ETS_TABLES=100000

erl \
	-noshell \
	-pa ebin \
	-s test_suite acceptance \
	-s init stop

