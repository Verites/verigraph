#!/usr/bin/env bash
#
# Scripting for generating HTML files for the api documentation of Verigraph.
# Must be run from the root directory of the verigraph project.
#
# Compiles and installs verigraph, generate the HTMLs for the api documentation,
# then, copy the files to doc folder on the root directory of the verigraph project.
# 
# The following must be installed:
#  - stack

USAGE="Usage:   $0 "
[ "$1" == "help" ] && echo $USAGE && exit 0

OUTPUT="./doc"
rm -r "$OUTPUT"

HTML_DIR=".stack-work/dist/x86_64-linux/Cabal-1.24.0.0/doc/html/verigraph"
rm -r "$HTML_DIR"
stack build --haddock --force-dirty  && stack install

cp -r "$HTML_DIR" "$OUTPUT"
