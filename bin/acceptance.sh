#!/bin/sh
cd `dirname $0`/..
exec erl -noshell -pa ebin -mnesia dir '"data"' -name probix -s test_suite run_tests -s init stop

