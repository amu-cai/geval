#!/bin/bash

ARENA=$1

wget --quiet 'https://gonito.net/list-challenges' -O - | perl -ne 'print "$1\n" if m{<a\s+\.challenge-link\s+href="https://gonito\.net/challenge/([^\"]+)">}' | while read challenge
do
    echo "---------------- $challenge ---------------------"

    challenge_dir="$ARENA/${challenge}-dont-peek"

    if [[ ! -d "${challenge_dir}" ]]
    then
        (cd $ARENA && git clone "ssh://gitolite@gonito.net/${challenge}-dont-peek")
    fi

    geval --validate --expected-directory "${challenge_dir}"

done
