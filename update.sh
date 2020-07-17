#!/bin/bash

FILES=(".vimrc"
       ".tmux.conf"
       ".emacs.d/init.el"
       ".emacs.d/config.org");
for FILE in "${FILES[@]}"; do
    cp $FILE "$HOME/";
done
