#!/bin/sh

usage()
{
cat << EOF
usage: $0 options

This script starts probix server

OPTIONS:
   -h      Show this message
   -N      Erlang node shortname. Default: probix

All options can be passed as environment variables:
    PROBIX_MODE, PROBIX_NODE_NAME, PROBIX_SERVER_IP, PROBIX_SERVER_PORT
EOF
}

while getopts “hm:N:I:P:M:D:T:” OPTION; do
    case "$OPTION" in
        h)  usage; exit ;;
        N)    PROBIX_NODE_NAME="$OPTARG" ;;
        \?) echo "Invalid option -$OPTARG" ;;
    esac
done

## Defaults
if [ -z ""$PROBIX_HOME ]; then
    PROBIX_HOME=`dirname $0`/..
fi

if [ -z ""$PROBIX_NODE_NAME ]; then
    PROBIX_NODE_NAME="probix";
fi

if [ -z ""$PROBIX_DATA_DIR ]; then
    PROBIX_DATA_DIR=$PROBIX_HOME/data;
fi


CMD="erl \
     +K true \
      -pa ebin \
      -pa deps/*/ebin \
      -boot start_sasl \
      -sname $PROBIX_NODE_NAME \
      -config priv/probix_app \
      -s reloader \
      -s start_probix"

## Setting running defaults
cd $PROBIX_HOME

$CMD 
