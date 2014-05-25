; GUI Font
(add-to-list 'default-frame-alist '(font . "PragmataPro-24" ))

(add-to-list 'load-path "~/.emacs.d/powerline")
(require 'powerline)
(powerline-evil-theme)

; Colors - launch emacs with TERM=xterm-16color for base16 and solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/base16-emacs.git")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized.git")
; (load-theme 'base16-eighties t)
; (load-theme 'base16-default t)
; (load-theme 'solarized-dark t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/moe-theme")
(add-to-list 'load-path "~/.emacs.d/moe-theme")
(require 'moe-theme)
(moe-dark)
;(setq moe-theme-mode-line-color 'purple)
;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
;(powerline-moe-theme)

; Packages
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

; SMEX - https://github.com/nonsequitur/smex
(add-to-list 'load-path "~/.emacs.d/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x

; EVIL (emacs vi layer) http://www.emacswiki.org/emacs/Evil
(add-to-list 'load-path "~/.emacs.d/undo-tree")
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(add-to-list 'load-path "~/.emacs.d/evil-surround")
(require 'surround)
(global-surround-mode 1)
(add-to-list 'load-path "~/.emacs.d/evil-matchit")
(require 'evil-matchit)
(global-evil-matchit-mode 1)

(define-key evil-normal-state-map (kbd "SPC SPC") 'smex)

; https://github.com/emacs-helm/helm
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)

; http://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(menu-bar-mode 0)
