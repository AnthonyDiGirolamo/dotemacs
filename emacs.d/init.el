(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (setq package-enable-at-startup nil)
(package-initialize)

;; Set a separate file for customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; (setq inhibit-startup-message t)

;; (require 'mouse)
;; (xterm-mouse-mode t)

(setq-default fill-column 80)

(add-to-list 'default-frame-alist '(font . "PragmataPro-24" ))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

;; Save Tempfiles in a temp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(defalias 'yes-or-no-p 'y-or-n-p) ;; no more typing out y.e.s.

;; (set-default 'show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace) ;; Erase trailing whitespace before save

;; Indentation
(setq-default c-basic-indent 2)
(setq-default tab-width 2)          ;; set tw=2
(setq-default indent-tabs-mode nil) ;; set expandtab

;; Save last location in a file
(require 'saveplace)
(setq-default save-place t)

;; Scroll just one line when hitting bottom of window
;; (setq scroll-step 1)
(setq scroll-conservatively 10000)

;; Rename file
;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
  (filename (buffer-file-name)))
  (if (not filename)
      (message "Buffer '%s' is not visiting a file!" name)
    (if (get-buffer new-name)
  (message "A buffer named '%s' already exists!" new-name)
      (progn  (rename-file name new-name 1)  (rename-buffer new-name)  (set-visited-file-name new-name)  (set-buffer-modified-p nil))))))

(setenv "ESHELL" (expand-file-name "~/Preferences/bin/eshell"))

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-auto-show-menu t)
(setq ac-auto-start t)
(setq ac-quick-help-delay 0.3)
(setq ac-quick-help-height 30)
(setq ac-show-menu-immediately-on-auto-complete t)
(ac-config-default)

;; ;; company-mode
;; (require 'company)
;; (setq company-idle-delay 0.2)
;; (setq company-minimum-prefix-length 1)
;; (setq company-show-numbers t)
;; (setq company-tooltip-limit 20)
;; (setq company-dabbrev-downcase nil)
;; (setq company-dabbrev-ignore-case nil)
;; (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
;; (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
;; (set-face-attribute 'company-preview nil :background "black")
;; (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
;; (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
;; (set-face-attribute 'company-scrollbar-fg nil :background "gray40")
;; (add-hook 'after-init-hook 'global-company-mode)

;; Colorschemes
(add-to-list 'custom-theme-load-path "~/.emacs.d/base16-emacs.git")
;; (load-theme 'base16-eighties t)
;; (load-theme 'base16-default t)
;; (load-theme 'solarized-dark t)
;; (load-theme 'wombat t)
;; (load-theme 'leuven t)
;; (load-theme 'subatomic256 t)
(require 'moe-theme)
(moe-theme-set-color 'blue)
(load-theme 'moe-dark t)
;; (moe-dark)
;; moe-theme mode-lines (doesn't support evil)
(setq moe-theme-mode-line-color 'blue)
;; ;; (Available colors: blue, orange, green ,magenta, yellow, purple, red, cyan, w/b.)
;; (powerline-moe-theme)

;; ;; smart-mode-line
(require 'smart-mode-line)
;; (sml/setup)
;; (sml/apply-theme 'dark)

;; Powerline
(require 'powerline)
(powerline-default-theme)
(load-file "~/.emacs.d/eyecandy.el")
(setq powerline-default-separator 'contour)
(setq powerline-height 25)
(powerline-spacemacs-imitation-theme)
;; (powerline-evil-vim-color-theme)


(require 'rainbow-delimiters)
;; (global-rainbow-delimiters-mode)
(add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)
(add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'shell-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" ","))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/idle-delay 0.2)
(guide-key-mode 1)
(require 'guide-key-tip)

(require 'ido)
(setq ido-enable-prefix nil)
(setq ido-use-virtual-buffers t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)
(setq ido-use-filename-at-point 'guess)
(ido-mode t)
(ido-everywhere t)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights.

;; SMEX
;; https://github.com/nonsequitur/smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ; old M-x

;; Evil (emacs vi layer)
;; http://www.emacswiki.org/emacs/Evil
(require 'evil)
(evil-mode 1)

;; Evil Addons
(require 'evil-surround)
(global-evil-surround-mode 1)
(add-hook 'web-mode-hook (lambda ()
                           (push '(?= . ("<%= " . " %>")) surround-pairs-alist)
                           (push '(?- . ("<% "  . " %>")) surround-pairs-alist)))

(require 'evil-matchit)
(global-evil-matchit-mode 1)

(require 'evil-leader)
(global-evil-leader-mode)

;; Line Bubble Functions
(defun move-line-up ()
  "move the current line up one line"
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  "move the current line down one line"
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun evil-move-lines (direction)
  "move selected lines up or down"
  (interactive)
  (evil-delete (region-beginning) (region-end))
  (evil-normal-state)
  (if (equal direction "up")
    (evil-previous-line)
    (evil-next-line))
  (evil-move-beginning-of-line)
  (evil-paste-before 1)
  (evil-visual-line (point) (- (point) (- (region-end) (region-beginning)))))

(defun evil-move-lines-up ()
  "move selected lines up one line"
  (interactive)
  (evil-move-lines "up"))

(defun evil-move-lines-down ()
  "move selected lines down one line"
  (interactive)
  (evil-move-lines "down"))

;; Keybindings
(evil-leader/set-leader ",")
(evil-leader/set-key
  "e" (kbd "C-x C-e")
  "E" (lambda() (interactive) (forward-char) (previous-line) (eval-print-last-sexp))
  "a" 'align-regexp
  "b" 'projectile-switch-to-buffer
  "d" 'dired
  "h" 'helm-mini
  "p" 'helm-projectile
  "P" (lambda() (interactive) (projectile-purge-dir-from-cache ".") (helm-projectile))
  "c" 'evil-commentary
  "n" 'rename-file-and-buffer
  "v" (lambda() (interactive) (evil-edit user-init-file)) )

(define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

(define-key evil-insert-state-map (kbd "C-e") 'emmet-expand-line)

;; Enter opens : prompt
(define-key evil-normal-state-map (kbd "C-m") 'evil-ex)

;; Ctrl-S saves in normal and insert mode
(define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "C-s") (lambda() (interactive) (save-buffer) (evil-normal-state)))

;; Bubble Lines
(define-key evil-normal-state-map (kbd "C-k") 'move-line-up)
(define-key evil-normal-state-map (kbd "C-j") 'move-line-down)

(define-key evil-visual-state-map (kbd "C-e") 'evil-move-lines-up)
(define-key evil-visual-state-map (kbd "C-n") 'evil-move-lines-down)

;; evil-nerdcomment
(add-hook 'c-mode-common-hook (lambda () (setq comment-start "// " comment-end "")))
(add-hook 'web-mode-hook (lambda () (setq comment-start "<!-- " comment-end " -->")))

;; key-chord
;; http://www.emacswiki.org/emacs/key-chord.el
(require 'key-chord)
(setq key-chord-two-keys-delay 0.2)
;; (key-chord-define evil-insert-state-map "jj" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
(key-chord-define evil-insert-state-map "ii" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
(key-chord-mode 1)

;; helm
;; https://github.com/emacs-helm/helm

;; Projectile
;; https://github.com/bbatsov/projectile
(projectile-global-mode t)
(setq projectile-require-project-root nil) ;; use projectile everywhere (no .projectile file needed)
(setq projectile-enable-caching t)

;; Markdown mode
(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Setting rbenv path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))
;; (add-to-list 'load-path "~/.emacs.d/xmpfilter")
;; (require 'rcodetools)
;; (global-set-key (kbd "C-c C-c") 'xmp)

;; Web Settings
(require 'web-mode)
(setq web-mode-engines-alist '(("liquid" . "\\.html\\'")))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; Python Settings
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; Ruby Settings
(setq ruby-deep-indent-paren nil)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; (push 'company-robe company-backends)

;; (eval-after-load 'inf-ruby
;;   `(add-to-list 'inf-ruby-implementations '("bundle console")))

;; Center Screen on search hit
;; http://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
(defadvice evil-search-next (after advice-for-evil-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))
(defadvice evil-search-previous (after advice-for-evil-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))

;; AceJump Mode
(define-key evil-normal-state-map (kbd "t") 'ace-jump-mode)

(evil-commentary-mode)

(global-git-gutter+-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init)
;;; init.el ends here
