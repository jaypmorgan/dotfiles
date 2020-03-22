#!/bin/bash

FILES=("${HOME}/.vimrc"
       "${HOME}/.emacs"
       "${HOME}/.config/i3"
       "${HOME}/.config/polybar"
       "${HOME}/.config/rofi"
       "${HOME}/.bash_aliases");
for FILE in "${FILES[@]}"; do
    echo "copying $FILE here";
    cp -r $FILE .;
done
