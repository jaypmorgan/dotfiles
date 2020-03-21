#!/bin/bash

FILES=(".vimrc"
       ".tmux.conf"
       ".bash_aliases"
       ".emacs");
for FILE in "${FILES[@]}"; do
    cp $FILE "${HOME}/.";
done
