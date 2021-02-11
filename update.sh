#!/bin/bash

FILES=(".emacs.d/init.el"
       ".emacs.d/config.org"
       ".xmonad/xmonad.hs");
for FILE in "${FILES[@]}"; do
    cp "$(basename -- $FILE)" "$HOME/$FILE";
done
