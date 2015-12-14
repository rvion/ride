#!/usr/bin/env bash
set -eux
for file in **/*.hs; do hlint $file --refactor --refactor-options="-i" && stylish-haskell --config=stylish-haskell.yaml -i $file ; done
