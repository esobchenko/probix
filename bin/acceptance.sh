#!/bin/sh
cd `dirname $0`/..

# because I want to run tests while my server is running
PROBIX_SERVER_PORT=1234

erl \
	-noshell \
	-pa ebin \
	-s test_suite acceptance \
	-s init stop

