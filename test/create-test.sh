#!/bin/bash

NAME="$1"
METRIC="$2"

DIR=$NAME

mkdir -p $DIR/$DIR/test-A
mkdir -p $DIR/$DIR-solution/test-A

echo '--metric' $METRIC > $DIR/$DIR/config.txt
touch $DIR/$DIR/test-A/expected.tsv
touch $DIR/$DIR-solution/test-A/out.tsv
