#!/bin/sh
emacs -Q --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"~/.emacs.d/README.org\")"
