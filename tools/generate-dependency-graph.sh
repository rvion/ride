#! /usr/local/bin/zsh
# see http://docs.haskellstack.org/en/stable/dependency_visualization.html?highlight=dot
stack dot | dot -Tpng -o doc/packages-deps-tree.png