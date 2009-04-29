#!/bin/sh
cd `dirname $0`/..
exec erl +K true -pa ebin -boot start_sasl -mnesia dir '"data"' -s probix -name probix -mnesia dump_log_write_threshold 50000 -mnesia dc_dump_limit 40

