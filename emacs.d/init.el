; Packages
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

; GUI Font
(add-to-list 'default-frame-alist '(font . "PragmataPro-24" ))

; Colors - launch emacs with TERM=xterm-16color for base16 and solarized
(add-to-list 'custom-theme-load-path "~/.emacs.d/base16-emacs.git")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized.git")
(load-theme 'base16-default)
; (load-theme 'solarized-dark t)

; EVIL (emacs vi layer)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-mini)

(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "0ba316caf0af7785622970574caa8d3cf758a43c2d7c63299fc521814d9a64ac" "7ad5c4ebefb34783a9cce63af92a25ebf76e06c47541124c5b3068379b6e1a49" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "0e121ff9bef6937edad8dfcff7d88ac9219b5b4f1570fd1702e546a80dba0832" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line-highlight ((t (:background "gray16" :foreground "RoyalBlue2")))))
