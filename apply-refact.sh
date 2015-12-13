#! /usr/local/bin/zsh

for file in **/*.hs; do
	echo linting $file
	stylish-haskell -i -v $file
	hlint -v --refactor --refactor-options="-i -v" $file;
done
# for file in **/*.hs; do ; done
