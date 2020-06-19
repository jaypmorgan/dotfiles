#!/bin/bash

FILES=("$HOME/.vimrc"
       "$HOME/.tmux.conf"
       "$HOME/.emacs.d/init.el");
for FILE in "${FILES[@]}"; do
    echo "copying $FILE here";
    cp --parents $FILE .;
done
