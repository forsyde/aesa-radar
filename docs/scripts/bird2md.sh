#!/bin/bash

linenum=0
codeblock=0
echo "" > "$2"
while IFS='' read -r line || [[ -n "$line" ]]; do
    linenum=$((linenum+1))
    if [ "$codeblock" -eq "0" ] && [[ "${line:0:2}" = '> ' ]]; then
	codeblock=1
	echo "~~~~ {.haskell .numberLines startFrom=\"$linenum\"}" >> "$2"
	echo "${line:2}" >> "$2"
    elif [ "$codeblock" -eq "1" ] && [[ "${line:0:2}" = '> ' ]]; then
	echo "${line:2}" >> "$2"
    elif [ "$codeblock" -eq "1" ]; then
	codeblock=0
	echo "~~~~~~~~~~~" >> "$2"
	echo "$line" >> "$2"
    elif [[ "${line:0:2}" = ' #' ]]; then
	echo "${line:1}" >> "$2"	
    else
	echo "$line" >> "$2"
    fi
done < "$1"
