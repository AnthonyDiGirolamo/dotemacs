;; GUI Font
(add-to-list 'default-frame-alist '(font . "PragmataPro-24" ))
(menu-bar-mode 0)
; (tool-bar-mode -1)

;; Save Tempfiles in a temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; no more typing out yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; Powerline
; (add-to-list 'load-path "~/.emacs.d/powerline")
; (require 'powerline)
; (powerline-evil-theme)

;; Colorschemes
(add-to-list 'custom-theme-load-path "~/.emacs.d/base16-emacs.git")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized.git")
; (load-theme 'base16-eighties t)
; (load-theme 'base16-default t)
; (load-theme 'solarized-dark t)

;; moe-theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/moe-theme")
(add-to-list 'load-path "~/.emacs.d/moe-theme")
(require 'moe-theme)
(moe-dark)
;; moe-theme mode-lines (doesn't support evil)
; (setq moe-theme-mode-line-color 'purple)
; ;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
; (powerline-moe-theme)

;; MELPA Package Repository
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; smart-mode-line
(setq sml/theme 'dark)
(require 'smart-mode-line)
(sml/setup)

; ;; guide-key
; (require 'guide-key)
; (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
; (guide-key-mode 1)  ; Enable guide-key-mode

;; SMEX - https://github.com/nonsequitur/smex
(add-to-list 'load-path "~/.emacs.d/smex")
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x

;; Evil (emacs vi layer) http://www.emacswiki.org/emacs/Evil
(add-to-list 'load-path "~/.emacs.d/undo-tree")
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
;; Evil Addons
(add-to-list 'load-path "~/.emacs.d/evil-surround")
(require 'surround)
(global-surround-mode 1)
(add-to-list 'load-path "~/.emacs.d/evil-matchit")
(require 'evil-matchit)
(global-evil-matchit-mode 1)
(add-to-list 'load-path "~/.emacs.d/evil-leader")
(require 'evil-leader)
(global-evil-leader-mode)

;; Evil Keybindings
(define-key evil-normal-state-map (kbd "SPC SPC") 'smex)

(evil-leader/set-leader ",")
(evil-leader/set-key
  "f" 'helm-projectile
  "h" 'helm-mini)

;; key-chord
;; http://www.emacswiki.org/emacs/key-chord.el
(add-to-list 'load-path "~/.emacs.d/key-chord")
(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

;; helm
;; https://github.com/emacs-helm/helm
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c f") 'helm-projectile)

;; Markdown mode
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; defadvice
;; http://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
(defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-rainbow-delimiters-mode t)
 '(sml/active-background-color "color-238")
 '(sml/inactive-background-color "color-235"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white" :weight bold))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "red" :weight bold))))
 '(sml/prefix ((t (:inherit sml/global :foreground "color-39")))))
