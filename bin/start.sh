#!/bin/sh
cd `dirname $0`
exec erl -pa ../ebin -pa /home/glorybox/devel/mochiweb/ebin -boot start_sasl -s probix -mnesia -name probix
