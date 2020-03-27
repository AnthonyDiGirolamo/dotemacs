#!/bin/sh
rm -f ~/.emacs.d/cache/autoload*
find ~/.emacs.d/elpa/ -iname "*.elc" -delete
emacs -Q --batch --eval '(byte-recompile-directory "~/.emacs.d/elpa/" 0 t)'
