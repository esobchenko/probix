#!/bin/sh
cd `dirname $0`/..
erl \
	-noshell \
	-pa ebin \
	-s test_suite acceptance \
	-s init stop

