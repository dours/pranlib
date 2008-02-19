#!/bin/sh

NAME=ocamlgraph
VERSION=0.99b
EXT=tar.gz

test -n "$1" || exit

test -d "$1" && cp -f m4/build.sh "$1/" && exit

test -d ../"$NAME-$VERSION" && mv ../"$NAME-$VERSION" "$1" && cp -f m4/build.sh "$1/" && exit

test -f ../"$NAME-$VERSION.$EXT" || wget -O ../"$NAME-$VERSION.$EXT" \
	"http://ocamlgraph.lri.fr/download/$NAME-$VERSION.$EXT"

test -f ../"$NAME-$VERSION.$EXT" && tar xzf ../"$NAME-$VERSION.$EXT" -C ..

test -d ../"$NAME-$VERSION" && mv ../"$NAME-$VERSION" "$1"

cp m4/build.sh "$1/"
