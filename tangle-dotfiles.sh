#!/bin/sh
emacs -Q --batch \
      --eval "(require 'org)" \
      --eval "(require 'ob-shell)" \
      --eval "(setq org-confirm-babel-evaluate nil)" \
      --eval "(org-babel-tangle-file \"~/.emacs.d/dotfiles/fish.org\")" \
      --eval "(org-babel-tangle-file \"~/.emacs.d/dotfiles/i3.org\")"
