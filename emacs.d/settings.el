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

(defun run-current-test ()
  (interactive)
  (let ((test-file-window (selected-window))
        (test-file-path   (buffer-file-name (current-buffer)))
        (test-command     (cond ((string-match "_spec.rb$" (buffer-file-name (current-buffer)))
                                 "~/.rbenv/shims/ruby ./bin/rspec ")
                                ((string-match "_test.py$" (buffer-file-name (current-buffer)))
                                 "py.test ")
                                (t
                                 "unknown_test_framework")))
        (rspec-buffer     (get-buffer-window "*rspec*")))
    ;; if the rspec buffer is open
    (if rspec-buffer
        ;; switch focus to it
        (select-window rspec-buffer)
      (progn
        ;; otherwise create a split and switch focus to it
        (select-window (split-window-right))
        ;; open the rspec-buffer
        (switch-to-buffer "*rspec*")))
    (erase-buffer)
    (shell-command
     (concat "cd " (projectile-project-root) " && "
             test-command
             test-file-path " &") "*rspec*")
    (evil-normal-state)
    (select-window test-file-window)))

;; (use-package zone
;;   :config
;;   (zone-when-idle 180)
;;   (defun zone-choose (pgm)
;;     "Choose a PGM to run for `zone'."
;;     (interactive
;;      (list
;;       (completing-read
;;        "Program: "
;;        (mapcar 'symbol-name zone-programs))))
;;     (let ((zone-programs (list (intern pgm))))
;;       (zone)))
;; )

;; (defun insert-tab-wrapper ()
;;   (interactive)
;;   (if (string-match "^[ \t]+$" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
;;       (insert (kbd "TAB"))
;;     (evil-complete-previous)))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

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

(use-package re-builder
  :init
  (setq reb-re-syntax 'string)
)

(use-package saveplace
  :config
  (setq-default save-place t)
  ;; (defadvice save-place-find-file-hook (after recenter activate)
  ;;   "Recenter after getting to saved place."
  ;;   (run-with-timer 0 nil (lambda (buf) (dolist (win (get-buffer-window-list buf nil t)) (with-selected-window win (recenter)))) (current-buffer)) )
)

(use-package auto-complete
  :diminish ""
  :config
  (setq ac-fuzzy-enable t)
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
  (load-theme 'moe-dark t)
)

;; (use-package leuven-theme
;;   :config
;;   (custom-theme-set-faces
;;    'leuven
;;    `(font-lock-keyword-face ((t (:foreground ,(face-foreground font-lock-builtin-face)
;;                                  :background ,(face-background font-lock-builtin-face)))))
;;    `(default ((t (:foreground "#333333" :background "#F5F5F5"))))
;;    `(fringe ((t (:foreground "#8B9B9B" :background "#F5F5F5")))))
;; )

;; (load-theme 'base16-eighties-dark t)

;; (load-theme 'cyberpunk)
;; (custom-theme-set-faces
;;  'cyberpunk
;;  `(default ((t (:background "#2d2d2d"))))
;;  `(fringe ((t (:background "#2d2d2d")))))

(use-package powerline
  :init
  (setq powerline-default-separator 'arrow)
  (cond ((eq system-type 'cygwin) (setq powerline-height 26))
        (t                        (setq powerline-height 28)))
)

(use-package airline-themes
  :load-path "airline-themes"
  :config
  ;; (load-theme 'airline-badwolf)
  (load-theme 'airline-light)
  ;; (load-theme 'airline-papercolor)
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
  (setq guide-key/guide-key-sequence '("C-x" "C-c" "C-w" ","))
  (setq guide-key/recursive-key-sequence-flag t)
  (setq guide-key/popup-window-position 'bottom)
  (setq guide-key/idle-delay 1.0)
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
  ;; (ido-mode t)
  ;; (ido-everywhere t)
  (ido-vertical-mode)
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
)

(use-package flx-ido
  :config
  (flx-ido-mode 1)
  (setq ido-use-faces nil) ;; disable ido faces to see flx highlights.
)

;; ;; SMEX https://github.com/nonsequitur/smex
;; (use-package smex
;;   :config
;;   (smex-initialize)
;;   :bind (("M-x" . smex)
;;          ("M-X" . smex-major-mode-commands)
;;          ("C-c C-c M-x" . execute-extended-command))
;; )

(use-package undo-tree
  :diminish ""
)

(use-package evil
  :config
  (evil-mode 1)

  (define-key  evil-normal-state-map            [escape]  'keyboard-quit)
  (define-key  evil-visual-state-map            [escape]  'keyboard-quit)
  (define-key  evil-emacs-state-map             [escape]  'keyboard-quit)
  (define-key  minibuffer-local-map             [escape]  'exit-minibuffer)
  (define-key  minibuffer-local-ns-map          [escape]  'exit-minibuffer)
  (define-key  minibuffer-local-completion-map  [escape]  'exit-minibuffer)
  (define-key  minibuffer-local-must-match-map  [escape]  'exit-minibuffer)
  (define-key  minibuffer-local-isearch-map     [escape]  'exit-minibuffer)

  (define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)

  (define-key evil-insert-state-map (kbd "C-e") 'emmet-expand-line)
  (define-key evil-insert-state-map (kbd "C-t") 'auto-complete)

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

  ;; don't use the clipboard - must yank/paste from "+ register
  (setq x-select-enable-clipboard nil)
  ;; Make sure undos are done atomically
  (setq evil-want-fine-undo 'no)
  ;; make * and # use the whole word
  (setq-default evil-symbol-word-search t)

  ;; Center Screen on search hit
  (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'makey-key-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-popup-mode)
  (add-to-list 'evil-normal-state-modes 'package-menu-mode)
)

(defun align-no-repeat (start end regexp)
  "Alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 nil))

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun align-to-comma (begin end)
  "Align region to comma signs"
  (interactive "r")
  (align-regexp begin end
                (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))

(defun align-to-colon (begin end)
  "Align region to colon"
  (interactive "r")
  (align-regexp begin end
                (rx ":" (group (zero-or-more (syntax whitespace))) ) 1 1 ))

(defun align-to-equals (begin end)
  "Align region to equal signs"
  (interactive "r")
  (align-regexp begin end
                (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "e" (kbd "C-x C-e")
    "E" 'evil-eval-print-last-sexp
    "an" 'align-no-repeat
    "aa" 'align-repeat
    "a:" 'align-to-colon
    "a=" 'align-to-equals
    "a," 'align-to-comma
    (kbd "C-m") (lambda()
                  (interactive)
                  (let ((current-prefix-arg 4)) ;; emulate C-u
                    (call-interactively 'align-regexp) ;; invoke align-regexp interactively
                  ))
    "g" 'magit-dispatch-popup ;; 'magit-status
    "d" 'dired
    "n" 'rename-file-and-buffer
    "v" (lambda() (interactive) (evil-edit user-init-file))
    "tt" 'run-current-test
    "k" 'kill-buffer
    "ag" 'helm-ag-project-root
    "x" 'helm-M-x
    "b" 'helm-mini
    "p" 'helm-projectile
    "P" (lambda() (interactive) (projectile-invalidate-cache (projectile-project-root)) (helm-projectile))
    "f" 'helm-flycheck
    "y" 'helm-show-kill-ring
    "r" 'helm-regexp
    "m" 'mu4e
    "w" 'ace-window
    "h" 'helm-descbinds
    "s" 'eshell-projectile-root
  )
)

(use-package org
  :init
  (setq org-default-notes-file "~/Dropbox/org/notes.org")
  :config
  (define-minor-mode evil-org-mode
    "Buffer local minor mode for evil-org"
    :init-value nil
    ;; :lighter " EvilOrg"
    :keymap (make-sparse-keymap) ; defines evil-org-mode-map
    :group 'evil-org
  )

  (add-hook 'org-mode-hook 'evil-org-mode) ;; only load with org-mode

  (defun clever-insert-item ()
    "Clever insertion of org item."
    (if (not (org-in-item-p))
        (insert "\n")
      (org-insert-item)))

  (defun evil-org-eol-call (fun)
    "Go to end of line and call provided function.
FUN function callback"
    (end-of-line)
    (funcall fun)
    (evil-append nil))

  (evil-define-key 'normal evil-org-mode-map
    "gh" 'outline-up-heading
    "gp" 'outline-previous-heading
    "gn" (if (fboundp 'org-forward-same-level) ;to be backward compatible with older org version
             'org-forward-same-level
           'org-forward-heading-same-level)
    "ge" (if (fboundp 'org-backward-same-level)
             'org-backward-same-level
           'org-backward-heading-same-level)
    "gl" 'outline-next-visible-heading
    "X" 'org-todo
    "H" 'org-beginning-of-line
    "L" 'org-end-of-line
    ;; "o" '(lambda () (interactive) (evil-org-eol-call 'clever-insert-item))
    "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))
    "$" 'org-end-of-line
    "^" 'org-beginning-of-line
    "<" 'org-metaleft
    ">" 'org-metaright
    "-" 'org-cycle-list-bullet
    (kbd "TAB") 'org-cycle
  )

  (mapc
   (lambda (state)
     (evil-define-key state evil-org-mode-map
       (kbd "M-l") 'org-metaright
       (kbd "M-h") 'org-metaleft
       (kbd "M-e") 'org-metaup
       (kbd "M-n") 'org-metadown
       (kbd "M-L") 'org-shiftmetaright
       (kbd "M-H") 'org-shiftmetaleft
       (kbd "M-E") 'org-shiftmetaup
       (kbd "M-N") 'org-shiftmetadown))
   '(normal insert))

  (evil-leader/set-key-for-mode 'org-mode
    "t"  'org-show-todo-tree
    "A"  'org-agenda
    "c"  'org-archive-subtree
    ;; "l"  'evil-org-open-links
    ;; "o"  'evil-org-recompute-clocks
  )
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

(use-package evil-jumper
  :config
  (global-evil-jumper-mode)
)

(use-package avy
  :config
  (setq avy-keys '(?t ?n ?s ?e ?d ?h ?r ?i ?a ?o ?b ?k ?g ?j ?v ?m ?p ?l))
  (setq avy-background t)
  (define-key evil-normal-state-map (kbd "t") 'avy-goto-char)
)

(use-package ace-window
  :config
  (setq aw-keys '(?t ?n ?s ?e ?d ?h ?r ?i ?a ?o ?b ?k ?g ?j ?v ?m ?p ?l))
)

(use-package ace-link
  :config
  (eval-after-load "eww"
    `(progn
       (define-key eww-link-keymap (kbd "f") 'ace-link-eww)
       (define-key eww-mode-map (kbd "f") 'ace-link-eww)))
)

;; key-chord http://www.emacswiki.org/emacs/key-chord.el
(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.2)
  ;; (key-chord-define evil-insert-state-map "jj" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
  (key-chord-define evil-insert-state-map "ii" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
  (key-chord-mode 1)
)

;; Projectile https://github.com/bbatsov/projectile
(use-package projectile
  :init
  (setq projectile-globally-ignored-directories '("vendor/ruby"))
  (setq projectile-require-project-root nil) ;; use projectile everywhere (no .projectile file needed)
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode t)
)

(use-package helm
  :diminish ""
  :bind (("M-x" . helm-M-x))
  :init
  (setq
   helm-mode-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-recentf-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-locate-fuzzy-match t
   helm-M-x-fuzzy-match t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-apropos-fuzzy-match t
   helm-lisp-fuzzy-completion t)
  (setq helm-split-window-in-side-p t)
  :config
  (helm-mode t)
  ;; (helm-adaptive-mode t)
  ;; (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
)
(use-package helm-config)
(use-package helm-projectile)
(use-package helm-descbinds
  :config
  (helm-descbinds-mode)
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
  (setenv "PATH"
          (concat "/usr/local/var/rbenv/shims:"
                  "/usr/local/var/rbenb/bin:"
                  (getenv "HOME") "/.rbenv/shims:"
                  (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))
  (setq exec-path
        (cons (concat (getenv "HOME") "/.rbenv/shims")
              (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'robe-mode-hook 'ac-robe-setup)
  ;; (push 'company-robe company-backends)

  (add-hook 'ruby-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
  ;; super word should handle the above
  ;; (add-hook 'ruby-mode-hook 'superword-mode)

  (eval-after-load 'inf-ruby
    `(add-to-list 'inf-ruby-implementations '("bundle console")))

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
  ;; (add-hook  'shell-mode-hook       'relative-line-numbers-mode)
  (add-hook  'emacs-lisp-mode-hook  'relative-line-numbers-mode)

  (defun abs-rel-numbers (offset)
    (if (= offset 0)
        ;; current line
        (format "%4d " (line-number-at-pos))
      ;; not the current line
      (format "%4d " (abs offset))
    )
  )

  (setq relative-line-numbers-format #'abs-rel-numbers)
)

(use-package flycheck
  :init (global-flycheck-mode)
)

(use-package flymake-ruby
  :config
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)
)

(use-package flymake-haml
  :config
  (add-hook 'haml-mode-hook 'flymake-haml-load)
)

(use-package magit
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
)

(use-package which-function
  :config
  (which-function-mode t)
)

(use-package dired-x
  :init
  (cond ((eq system-type 'darwin)
         (setq insert-directory-program "/usr/local/bin/gls"))
        (t
         (setq insert-directory-program "ls")))
)

(use-package discover
  :config
  (global-discover-mode 1)
)

(use-package mu4e
  :load-path "/usr/local/Cellar/mu/0.9.2/share/emacs"
  :config
  (define-key mu4e-headers-mode-map (kbd "e") 'mu4e-headers-prev)
  (define-key mu4e-view-mode-map (kbd "e") 'mu4e-view-headers-prev)

  (setq mu4e-use-fancy-chars nil)
  (setq mu4e-attachment-dir "~/Download")
  (setq mu4e-view-show-images t)

  (setq
   mu4e-maildir       "~/Mail"
   mu4e-sent-folder   "/Sent Items"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder  "/Deleted Items"
   mu4e-refile-folder "/Archive")
  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"         . ?i)
           ("/Sent Items"    . ?s)
           ("/Deleted Items" . ?d)
           ("/uag"           . ?u)))

  (setq mu4e-html2text-command "w3m -T text/html")
  ;; (setq mu4e-html2text-command "pandoc -f html -t org")

  (load "~/.emacs.d/email-settings.el")

  (setq mu4e-compose-complete-ignore-address-regexp "no-?reply\|via RT")

  (add-to-list 'mu4e-bookmarks '("flag:flagged" "Flagged" ?f))
)

(use-package smtpmail
  :config
  (setq smtpmail-queue-mail t)
  (setq smtpmail-queue-dir  "~/Mail/queue/cur")

  (setq message-kill-buffer-on-exit t)
)

(use-package eshell
  :init
  (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
  (setenv "PAGER" "cat")
  ;; (setq eshell-buffer-shorthand t)
  :config
  (defun eshell-projectile-root ()
    "open eshell in projectile-root"
    (interactive)
    (select-window (split-window-below))
    (eshell)
    (rename-buffer (concat "*eshell:" (projectile-project-name) "*"))
    (insert (concat "cd " (projectile-project-root)))
    (eshell-send-input))

  (defalias 'e 'find-file-other-window)
  (defalias 'emacs 'find-file)

  ;; Turn on helm completion and history
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map
                (kbd "M-p")
                'helm-eshell-history)))

  (defun curr-dir-git-branch-string (pwd)
    "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (when (and (eshell-search-path "git")
               (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
        (if (> (length git-output) 0)
            (concat (substring git-output 0 -1))
          "(no branch)"))))

  (setq eshell-prompt-function
        (lambda ()
          (concat
           (propertize (concat " " (eshell/whoami) " ") 'face
                       `(:foreground ,(face-foreground 'airline-insert-outer) :background ,(face-background 'airline-insert-outer)))

           (propertize (concat (char-to-string airline-utf-glyph-separator-left) " ") 'face
                       `(:foreground ,(face-background 'airline-insert-outer) :background ,(face-background 'airline-insert-inner)))

           (propertize (concat (shorten-directory (eshell/pwd) 40) " ") 'face
                       `(:foreground ,(face-foreground 'airline-insert-inner) :background ,(face-background 'airline-insert-inner)))

           (propertize (concat (char-to-string airline-utf-glyph-separator-left) " ") 'face
                       `(:foreground ,(face-background 'airline-insert-inner) :background ,(face-background 'airline-insert-center)))

           (propertize (concat (curr-dir-git-branch-string (eshell/pwd)) " ") 'face
                       `(:foreground ,(face-foreground 'airline-insert-center) :background ,(face-background 'airline-insert-center)))

           (propertize (concat (char-to-string airline-utf-glyph-separator-left)) 'face
                       `(:foreground ,(face-background 'airline-insert-center)))

           (propertize " $ " 'face `())
           )))

  (setq eshell-highlight-prompt t)
  (setq eshell-prompt-regexp "^ [^#$]* [#$] ")

  (defun eshell/x ()
    "Closes the EShell session and gets rid of the EShell window."
    (kill-buffer)
    (delete-window))
)

(use-package em-smart
  :init
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
)

(provide 'settings)
;;; settings.el ends here
