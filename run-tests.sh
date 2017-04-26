#!/bin/sh

stack test --test-arguments "$(cat .hspec)"
