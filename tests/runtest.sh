#!/bin/sh
update=false
hexopt="--output-coding=utf8 --charwidth=1"
if [ "$1" = --update ]; then
    update=true
fi
LS_COLORS=1 export LS_COLORS # force ANSI sequences
test -f hex.pl && cd tests
for f in *.test
do
    out="${f%.test}.out"
    if ../hex.pl $hexopt -dt "$f" | diff -u "$out" /dev/stdin;
    then
	printf "%s: ok\n" "$f"
    else
	if $update; then
	    ../hex.pl $hexopt -dt "$f" > "$out"
	    printf "%s: UPDATED\n" "$f"
	else
	    printf "%s: NG\n" "$f"
	fi
    fi	
done
