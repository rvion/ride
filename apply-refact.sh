#! /usr/local/bin/zsh

for file in **/*.hs; do
	echo linting $file
	hlint -v --refactor --refactor-options="-i" $file;
done
# for file in **/*.hs; do ; done
