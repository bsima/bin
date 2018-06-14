#!/bin/sh
#
# A non-bash version of
#
# http://tldp.org/HOWTO/Bash-Prompt-HOWTO/x329.html
#
# Original author:
# https://lobste.rs/s/uyw4pq/lobsters_battlestations_screenshots#c_5qrnht

text=${1:-gYw}
textwidth=$((${#text} < 3 ? 3 : ${#text}))

printtable () {
	printf '\n%8s' ' '
	for bg in $(bgs); do
		printcol '' $bg ''
	done
	printf '\n'
	for fg in $(fgs); do
		printrow $fg
		printrow 1\;$fg
	done
	printf '\n'
}

printcol () {
	printf "%s  %${textwidth}s  %s " "${@}"
}

printrow () {
	printf '%6s  ' "${1}"
	for bg in $(bgs); do
		printf "\033[%s\033[%s  %${textwidth}s  \033[0m " "${1}" $bg "${text}"
	done
	printf '\n'
}

bgs () {
	(printf 49\\n; seq 40 47) | sed s/$/m/
}

fgs () {
	(printf 39\\n; seq 30 37) | sed s/$/m/
}

printtable
