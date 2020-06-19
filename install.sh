#!/bin/bash

FILES=(".vimrc"
       ".tmux.conf"
       ".emacs.d")
for FILE in "${FILES[@]}"; do
    cp -r $FILE "${HOME}/.";
done
