#!/bin/bash
FILE=docker-compose.yml
docker-compose -f $FILE down
docker-compose -f $FILE build
docker-compose -f $FILE up

IMAGE="$(docker ps -q -f ancestor=pcs3556-computationallogic_pcs3556_clojure)"

if [[ $IMAGE ]] ; then
	if [[ "$1" ]] ; then
		SRCDIR="$( cd $1 && pwd )"
	else
		GITDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
		SRCDIR="$GITDIR"
	fi

	if [[ "$2" ]]; then
		docker run --rm -ti -v "$SRCDIR":/usr/app/src --device="$2":/dev/ttyS0 pcs3556-computationallogic_pcs3556_clojure
	else
		docker run --rm -ti -v "$SRCDIR":/usr/app/src pcs3556-computationallogic_pcs3556_clojure
	fi
fi