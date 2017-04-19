#!/usr/bin/env bash
#
# Scripting for generating HTML files for the api documentation of Verigraph.
# Must be run from directory tree of the verigraph project.
#
# Compiles and installs verigraph, generate the HTMLs for the api documentation,
# then, copy the files to doc folder on the root directory of the verigraph project.
# 
# The following must be installed:
#  - stack

USAGE="Usage:   $0 "
[ "$1" == "help" ] && echo $USAGE && exit 0

#Get the path to the project root
PROJECT_ROOT_PATH="$(stack path --project-root | tr -d "\n")"

#Get the path to the local doc directory
LOCAL_DOC_PATH="$(stack path --local-doc-root | tr -d "\n")"

#Get package name and version
PACKAGE_NAME="$(stack query locals | grep ":$" | tr -d ": | \n")"
PACKAGE_VERSION="$(stack query locals | grep "version:"| tr -d " *version: | ' | \n")"

#Build the specific package version doc directory
HTML_DIR="$LOCAL_DOC_PATH/$PACKAGE_NAME-$PACKAGE_VERSION"


#Build the specific output directory on the project root
OUTPUT="$PROJECT_ROOT_PATH/doc"

#Clean the directorys to avoid extract of old files
rm -r "$HTML_DIR";
rm -r "$OUTPUT";

stack build --haddock --no-haddock-deps --force-dirty  && stack install  && cp -r "$HTML_DIR" "$OUTPUT"
