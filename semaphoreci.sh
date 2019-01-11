#!/bin/bash

STACK=$SEMAPHORE_CACHE_DIR/stack

setup() {
	if [ ! -e "$STACK" ]
	then
		curl -L https://get.haskellstack.org/stable/linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C $SEMAPHORE_CACHE_DIR '*/stack'
	fi

	$STACK --no-terminal setup
	$STACK --no-terminal test --only-snapshot
}

test() {
	$STACK --no-terminal test --pedantic
}

$1
