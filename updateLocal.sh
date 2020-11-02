#!/bin/bash

FILES=("$HOME/.vimrc"
       "$HOME/.tmux.conf"
       "$HOME/.emacs.d/init.el"
       "$HOME/.emacs.d/config.org"
       "$HOME/.emacs.d/config.el");
for FILE in "${FILES[@]}"; do
    echo "copying $FILE here";
    cp $FILE .;
done
