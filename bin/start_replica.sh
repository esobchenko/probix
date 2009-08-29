#!/bin/sh
cd `dirname $0`/..
erl \
	+K true \
	-pa ebin \
	-boot start_sasl \
	-mnesia dir '"data"' \
	-mnesia dump_log_write_threshold 50000 \
	-mnesia dc_dump_limit 40 \
	-name probix2 \
	-s probix start_replica $1

