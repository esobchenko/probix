#!/bin/sh
cd `dirname $0`/..
exec erl -pa ebin -boot start_sasl -s probix -mnesia dir '"data"' -name probix
