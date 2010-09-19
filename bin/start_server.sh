#!/bin/sh

usage()
{
cat << EOF
usage: $0 options

This script starts probix server in master or replica mode

OPTIONS:
   -h      Show this message
   -N      Erlang node shortname. Default: probix
   -I      Mochiweb interface address. Default: 0.0.0.0
   -P      Mochiweb port. Default: 8000
   -D      Data directory. Directory for mnesia data

All options can be passed as environment variables:
    PROBIX_MODE, PROBIX_NODE_NAME, PROBIX_SERVER_IP, PROBIX_SERVER_PORT
EOF
}

while getopts “hm:N:I:P:M:D:T:” OPTION; do
    case "$OPTION" in
        h)  usage; exit ;;
        m)    PROBIX_MODE="$OPTARG" ;;
        M)    PROBIX_MASTER_NODE="$OPTARG" ;;
        N)    PROBIX_NODE_NAME="$OPTARG" ;;
        I)    PROBIX_SERVER_IP="$OPTARG" ;;
        P)    PROBIX_SERVER_PORT="$OPTARG" ;;
        D)    PROBIX_DATA_DIR="$OPTARG" ;;
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
      -conf priv/probix_app
      -s start_probix"

## Setting running defaults
export PROBIX_TEST_MODE=0
export ERL_MAX_ETS_TABLES=100000

if [ -n "$PROBIX_SERVER_PORT" ]; then
    export PROBIX_SERVER_PORT
fi

if [ -n "$PROBIX_SERVER_IP" ]; then
    export PROBIX_SERVER_IP
fi

cd $PROBIX_HOME

$CMD 
