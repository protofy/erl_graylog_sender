#!/bin/bash

if [ "${OSTYPE:0:6}" = "darwin" ]; then
	GL=$(which greadlink)
	BASEDIR="$(dirname $0)/.."
	if [ $? -eq 0 ]; then
		if [ $(greadlink --version | head -c 8) = "readlink" ]; then
			BASEDIR=$(greadlink -f "$(dirname $0)/..")
		fi
	fi
else
	BASEDIR=$(readlink -f "$(dirname $0)/..")
fi

NODE="-name erl_graylog_sender_dev@127.0.0.1"

cd $BASEDIR/dev

erlc -I $BASEDIR/deps -I $BASEDIR/src *.erl

cd $BASEDIR

erl $NODE -pa $BASEDIR/deps/*/ebin $BASEDIR/ebin $BASEDIR/src/*/*/deps/*/ebin $BASEDIR/src/*/*/ebin $BASEDIR/dev
