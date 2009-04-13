#!/bin/bash
JARS=$HOME/Library/Clojure/lib
CP=.:./src:$JARS/clojure.jar:$JARS/clojure-contrib.jar
TEST="src/mreid/minilight/test/all.clj"
MAIN="(mreid.minilight.test.all/-main)"

java -cp $CP clojure.main -i $TEST -e "$MAIN"