#!/usr/bin/env bash
#
# Script for generating PNGs from the expanded state space of a graph grammar.
# Must be run from the root directory of the verigraph project.
#
# Compiles and installs verigraph, runs the state space generation, then uses graphviz to
# generate PNGs for the state space graph as well as each individual state.
#
# The following must be installed:
#  - stack
#  - graphviz

USAGE="Usage:   $0 [grammar [depth]]"
[ "$1" == "help" ] && echo "$USAGE" && exit 0

depth=$2
[ -z "$depth" ] && depth=5

example=$1
[ -z "$example" ] && example="grammars/ADTs/createList.ggx"

stack install
mkdir -p test-output/
rm -f test-output/*

[ "$(verigraph-mcheck -d$depth -otest-output "$example")" ] || exit 1

for file in test-output/*.dot
do
  dot "$file" -Tpng > "$file.png"
done
