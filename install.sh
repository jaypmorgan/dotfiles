#!/bin/bash

FILES=(".vimrc"
       ".tmux.conf"
       ".bash_aliases"
       ".emacs.d/init.el")
for FILE in "${FILES[@]}"; do
    cp $FILE "${HOME}/.";
done
echo 'source ~/.bash_aliases' >> ~/.bashrc
