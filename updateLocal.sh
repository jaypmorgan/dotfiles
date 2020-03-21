#!/bin/bash

FILES=("${HOME}/.vimrc"
       "${HOME}/.emacs"
       "${HOME}/.config/i3/config"
       "${HOME}/.bash_aliases");
for FILE in "${FILES[@]}"; do
    echo "copying $FILE here";
    cp $FILE .;
done
