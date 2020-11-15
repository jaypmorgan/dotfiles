#!/bin/bash

FILES=("$HOME/.emacs.d/init.el"
       "$HOME/.emacs.d/config.org"
       "$HOME/.emacs.d/config.el"
       "$HOME/.xmonad/xmonad.hs");
for FILE in "${FILES[@]}"; do
    echo "copying $FILE here";
    cp $FILE .;
done
