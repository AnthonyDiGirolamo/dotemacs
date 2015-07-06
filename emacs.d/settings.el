;; (require 'mouse)
;; (xterm-mouse-mode t)

(setq-default fill-column 80)

(cond ((eq system-type 'cygwin)
       (add-to-list 'default-frame-alist '(font . "PragmataPro-13" )))
      (t
       (add-to-list 'default-frame-alist '(font . "PragmataPro-22" ))))

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

;; Scroll just one line when hitting bottom of window
;; (setq scroll-step 1)
(setq scroll-conservatively 10000)

(setenv "ESHELL" (expand-file-name "~/Preferences/bin/eshell"))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

; ;; Save last location in a file
(use-package saveplace
  :config
  (setq-default save-place t)
  ;; (defadvice save-place-find-file-hook (after recenter activate)
  ;;   "Recenter after getting to saved place."
  ;;   (run-with-timer 0 nil (lambda (buf) (dolist (win (get-buffer-window-list buf nil t)) (with-selected-window win (recenter)))) (current-buffer)) )
)

;; auto-complete
(use-package auto-complete
  :diminish ""
  :config
  (setq ac-auto-show-menu t)
  (setq ac-auto-start t)
  (setq ac-quick-help-delay 0.3)
  (setq ac-quick-help-height 30)
  (setq ac-show-menu-immediately-on-auto-complete t)
  (ac-config-default)
)
(use-package auto-complete-config)

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

(use-package moe-theme
  :config
  (moe-theme-set-color 'blue)
  (load-theme 'moe-dark t)
  ;; (moe-dark)
)

;; (use-package leuven-theme
;;   :config
;;   (custom-theme-set-faces
;;    'leuven
;;    `(default ((t (:foreground "#333333" :background "#F5F5F5"))))
;;    `(fringe ((t (:foreground "#8B9B9B" :background "#F5F5F5")))))
;; )

;; Powerline
(use-package powerline
  :config
  ;; (powerline-default-theme)
  (setq powerline-default-separator 'arrow)
  (setq powerline-height 25)
)

(add-to-list 'load-path "~/.emacs.d/airline-themes")
(use-package airline-themes
  :config
  (airline-theme-badwolf)
  ;; (airline-theme-papercolor)
)

(use-package rainbow-delimiters
  :config
  ;; (global-rainbow-delimiters-mode)
  (add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'shell-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
)

(use-package guide-key
  :diminish ""
  :config
  (setq guide-key/guide-key-sequence '("C-x" "C-c" ","))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/idle-delay 0.2)
  (guide-key-mode 1)
)

(use-package guide-key-tip)

(use-package ido
  :config
  (setq ido-enable-prefix nil)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-use-filename-at-point 'guess)
  (ido-mode t)
  (ido-everywhere t)
  (ido-vertical-mode)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
)

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-use-faces nil) ;; disable ido faces to see flx highlights.
)

;; SMEX https://github.com/nonsequitur/smex
(use-package smex
  :config
  (smex-initialize)
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))
)

(use-package undo-tree
  :diminish ""
)

(use-package evil
  :config
  ;; (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

  (define-key evil-insert-state-map (kbd "C-e") 'emmet-expand-line)

  (define-key evil-motion-state-map "n" 'evil-next-line)
  (define-key evil-motion-state-map "e" 'evil-previous-line)
  (define-key evil-motion-state-map "k" 'evil-ex-search-next)
  (define-key evil-motion-state-map "K" 'evil-ex-search-previous)

  ;; Enter opens : prompt
  (define-key evil-normal-state-map (kbd "C-m") 'evil-ex)
  (define-key evil-visual-state-map (kbd "C-m") 'evil-ex)

  ;; Ctrl-S saves in normal and insert mode
  (define-key evil-normal-state-map (kbd "C-s") 'save-buffer)
  (define-key evil-insert-state-map (kbd "C-s") (lambda() (interactive) (save-buffer) (evil-normal-state)))

  ;; Bubble Lines
  (define-key evil-normal-state-map (kbd "C-e") 'move-line-up)
  (define-key evil-normal-state-map (kbd "C-n") 'move-line-down)
  (define-key evil-visual-state-map (kbd "C-e") 'evil-move-lines-up)
  (define-key evil-visual-state-map (kbd "C-n") 'evil-move-lines-down)

  ;; Make sure undos are done atomically
  (setq evil-want-fine-undo 'no)

  (evil-mode 1)

  ;; Center Screen on search hit
  ;; http://bling.github.io/blog/2013/10/27/emacs-as-my-leader-vim-survival-guide/
  (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (defadvice package-list-packages (after advice-for-package-list-packages activate)
    "Enter evil normal state after opening package-list-packages"
    (evil-normal-state))
)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  (add-hook 'web-mode-hook (lambda ()
                             (push '(?= . ("<%= " . " %>")) surround-pairs-alist)
                             (push '(?- . ("<% "  . " %>")) surround-pairs-alist)))
)

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-commentary
  :diminish ""
  :config
  (evil-commentary-mode)
)

;; Rename file https://sites.google.com/site/steveyegge2/my-dot-emacs-file
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

(defun evil-eval-print-last-sexp ()
  "eval print when in evil-normal-state"
  (interactive) (forward-char) (previous-line) (eval-print-last-sexp))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "e" (kbd "C-x C-e")
    "E" 'evil-eval-print-last-sexp
    "g" 'magit-status
    "a" (lambda()
          (interactive)
          (let ((current-prefix-arg 4)) ;; emulate C-u
            (call-interactively 'align-regexp) ;; invoke align-regexp interactively
            ))
    "b" 'projectile-switch-to-buffer
    "d" 'dired
    "h" 'helm-mini
    "p" 'helm-projectile
    "P" (lambda() (interactive) (projectile-invalidate-cache) (helm-projectile))
    "c" 'evil-commentary
    "n" 'rename-file-and-buffer
    "v" (lambda() (interactive) (evil-edit user-init-file)))
)

;; AceJump Mode
(use-package ace-jump-mode
  :config
  (setq ace-jump-mode-case-fold t)
  (setq ace-jump-mode-move-key
      (loop for i from ?a to ?z collect i))
  (define-key evil-normal-state-map (kbd "t") 'ace-jump-mode)
)

;; ;; evil-nerdcomment
;; (add-hook 'c-mode-common-hook (lambda () (setq comment-start "// " comment-end "")))
;; (add-hook 'web-mode-hook (lambda () (setq comment-start "<!-- " comment-end " -->")))

;; key-chord http://www.emacswiki.org/emacs/key-chord.el
(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.2)
  ;; (key-chord-define evil-insert-state-map "jj" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
  (key-chord-define evil-insert-state-map "ii" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
  (key-chord-mode 1)
)

;; helm https://github.com/emacs-helm/helm
(use-package helm
  ;; :diminish ""
  ;; :config
  ;; (require 'helm-config)
  ;; (helm-mode 1)
)


;; Projectile https://github.com/bbatsov/projectile
(use-package projectile
  :defer t
  :init
  (setq projectile-globally-ignored-directories '("vendor/ruby"))
  :config
  (projectile-global-mode t)
  (setq projectile-require-project-root nil) ;; use projectile everywhere (no .projectile file needed)
  (setq projectile-enable-caching t)
)

;; Markdown mode
(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
)

;; Web Settings
(use-package web-mode
  :config
  (setq web-mode-engines-alist '(("liquid" . "\\.html\\'")))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
)

;; Python Settings
(use-package jedi
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
)

;; Ruby Settings
(use-package robe
  :init
  (setq ruby-deep-indent-paren nil)
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  ;; (push 'company-robe company-backends)
  ;; (eval-after-load 'inf-ruby
  ;;   `(add-to-list 'inf-ruby-implementations '("bundle console")))
  ;; Setting rbenv path
  (setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
  (setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))
  ;; (add-to-list 'load-path "~/.emacs.d/xmpfilter")
  ;; (require 'rcodetools)
  ;; (global-set-key (kbd "C-c C-c") 'xmp)
)

(use-package relative-line-numbers
  :diminish ""
  :config
  ;; (global-relative-line-numbers-mode)
  (add-hook  'ruby-mode-hook        'relative-line-numbers-mode)
  (add-hook  'c-mode-common-hook    'relative-line-numbers-mode)
  (add-hook  'python-mode-hook      'relative-line-numbers-mode)
  (add-hook  'shell-mode-hook       'relative-line-numbers-mode)
  (add-hook  'emacs-lisp-mode-hook  'relative-line-numbers-mode)

  (defun abs-rel-numbers (offset)
    (if (= offset 0)
        ;; current line
        (format "%4d " (line-number-at-pos))
      ;; not the current line
      (format "%4d " (abs offset))
      ))

  (setq relative-line-numbers-format #'abs-rel-numbers)
)

(use-package flycheck
  :init (global-flycheck-mode)
)

(use-package magit
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
)

(use-package which-function
  :config
  (which-function-mode t)
)

(provide 'settings)
;;; settings.el ends here
