#!/bin/sh

usage()
{
cat << EOF
usage: $0 options

This script starts probix server in master or replica mode

OPTIONS:
   -h      Show this message
   -m      Running mode: master or replica. Default: master
   -M      Master node. Relevant only for 'replica' mode.
   -N      Erlang node shortname. Default: probix
   -I      Mochiweb interface address. Default: 0.0.0.0
   -P      Mochiweb port. Default: 8000
   -D      Data directory. Directory for mnesia data

All options can be passed as environment variables:
    PROBIX_MODE, PROBIX_NODE_NAME, PROBIX_SERVER_IP, PROBIX_SERVER_PORT,
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

if [ -z ""$PROBIX_MODE ]; then
    PROBIX_MODE="master";
fi

if [ -z ""$PROBIX_DATA_DIR ]; then
    PROBIX_DATA_DIR=$PROBIX_HOME/data;
fi

if [ -z ""$PROBIX_STORAGE_TYPE ]; then
    PROBIX_STORAGE_TYPE="disc_copies";
fi


## Checking arguments consistency
if [ ""$PROBIX_MODE = "replica" -a -z ""$PROBIX_MASTER_NODE ]; then
    echo "";
    echo "Please specify MASTER_NODE with -M option or set environment variable for 'replica' mode";
    echo "";
    usage;
    exit;
fi



CMD="erl \
     +K true \
      -pa ebin \
      -boot start_sasl \
      -mnesia dir '$PROBIX_DATA_DIR' \
      -mnesia dump_log_write_threshold 50000 \
      -mnesia dc_dump_limit 40 \
      -sname $PROBIX_NODE_NAME \
      -setcookie probix \
      -s probix"

## Setting running defaults
export PROBIX_TEST_MODE=0
export ERL_MAX_ETS_TABLES=100000

if [ -n "$PROBIX_SERVER_PORT" ]; then
    export PROBIX_SERVER_PORT
fi

if [ -n "$PROBIX_SERVER_IP" ]; then
    export PROBIX_SERVER_IP
fi

if [ -n "$PROBIX_STORAGE_TYPE" ]; then
    export PROBIX_STORAGE_TYPE
fi

cd $PROBIX_HOME

if [ ""$PROBIX_MODE = "master" ]; then
    $CMD start_master $PROBIX_STORAGE_TYPE
elif [ ""$PROBIX_MODE = "replica" ]; then
    $CMD start_replica $PROBIX_STORAGE_TYPE $PROBIX_MASTER_NODE
fi


