#!/bin/bash

FILES=(".vimrc"
       ".tmux.conf"
       ".emacs.d/init.el");
for FILE in "${FILES[@]}"; do
    cp $FILE "$HOME/";
done
