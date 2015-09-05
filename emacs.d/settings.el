(require 'mouse)
(xterm-mouse-mode t)

(setq gc-cons-threshold 20000000)

(setq-default fill-column 80)

(cond ((eq system-type 'cygwin)
       (add-to-list 'default-frame-alist '(font . "PragmataPro-13" )))
      ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(font . "PragmataPro-15" )))
      (t
       (add-to-list 'default-frame-alist '(font . "PragmataPro-22" ))))

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))

;; Save Tempfiles in a temp dir
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Stop making backup files
(setq make-backup-files nil)

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

(defun run-current-test (&optional line-no only-run-file)
  (interactive)
  (let ((test-file-window (selected-window))
        (test-file-path   (buffer-file-name (current-buffer)))
        (test-command     (cond (only-run-file "")
                                ((string-match "_spec.rb$" (buffer-file-name (current-buffer)))
                                 "~/.rbenv/shims/ruby ./bin/rspec ")
                                ((string-match "_test.py$" (buffer-file-name (current-buffer)))
                                 "py.test -v --doctest-modules ")
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
        (evil-window-move-far-right)
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
  (interactive (list (read-string "New name:" (buffer-name))))
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

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

;; (if window-system
;;     (use-package leuven-theme
;;       :config
;;       (custom-theme-set-faces
;;        'leuven
;;        `(font-lock-keyword-face ((t (:foreground ,(face-foreground font-lock-builtin-face)
;;                                                  :background ,(face-background font-lock-builtin-face)))))
;;        `(default ((t (:foreground "#333333" :background "#F5F5F5"))))
;;        `(fringe ((t (:foreground "#8B9B9B" :background "#F5F5F5"))))))
  (use-package moe-theme
    :config
    (load-theme 'moe-dark t)
    (custom-theme-set-faces
     'moe-dark
     `(org-level-2 ((t (:foreground "#ff8700"))))
     `(org-level-3 ((t (:foreground "#a1db00"))))

     `(ediff-current-diff-A ((t (:foreground "gray33" :background "#FFDDDD"))))
     `(ediff-current-diff-B ((t (:foreground "gray33" :background "#DDFFDD"))))
     `(ediff-current-diff-C ((t (:foreground "black" :background "#00afff"))))

     `(ediff-even-diff-A ((t (:background "#4e4e4e"))))
     `(ediff-even-diff-B ((t (:background "#4e4e4e"))))
     `(ediff-even-diff-C ((t (:background "#4e4e4e"))))

     `(ediff-fine-diff-A ((t (:foreground "#af0000" :background "#FFAAAA"))))
     `(ediff-fine-diff-B ((t (:foreground "#008000" :background "#55FF55"))))
     `(ediff-fine-diff-C ((t (:foreground "black" :background "#ffff5f"))))

     `(ediff-odd-diff-A ((t (:background "#4e4e4e"))))
     `(ediff-odd-diff-B ((t (:background "#4e4e4e"))))
     `(ediff-odd-diff-C ((t (:background "#4e4e4e"))))
     `(ediff-odd-diff-Ancestor ((t (:background "#4e4e4e"))))
     )
  )
;; )

;; (if window-system
;;     (load-theme 'base16-atelierdune-dark t)
;;   (load-theme 'base16-shell-dark t))

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
  ;; (if window-system
  ;;     (load-theme 'airline-base16-gui-dark t)
  ;;   (load-theme 'airline-base16-shell-dark t))
  (if window-system
      (load-theme 'airline-light t)
    (load-theme 'airline-base16-shell-dark t))
  ;; (load-theme 'airline-badwolf)
  ;; (load-theme 'airline-light)
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
  (setq guide-key/guide-key-sequence '("C-h" "C-x" "C-c" "C-w" ","))
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

  (define-key evil-normal-state-map (kbd "C-w N") 'evil-window-move-very-bottom)
  (define-key evil-normal-state-map (kbd "C-w E") 'evil-window-move-very-top)
  (define-key evil-normal-state-map (kbd "C-w H") 'evil-window-move-far-left)
  (define-key evil-normal-state-map (kbd "C-w L") 'evil-window-move-far-right)

  (define-key evil-motion-state-map "n" 'evil-next-visual-line)
  (define-key evil-motion-state-map "e" 'evil-previous-visual-line)
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

  ;; don't use the clipboard
  (setq x-select-enable-clipboard 1)
  ;; Make sure undos are done atomically
  (setq evil-want-fine-undo 'no)
  ;; make * and # use the whole word
  (setq-default evil-symbol-word-search t)
  ;; don't move back one charachter when exiting insert
  (setq evil-move-cursor-back nil)

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

(defun align-interactively ()
  "invoke align-regexp interactively"
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'align-regexp)))

(defun helm-projectile-invalidate-cache ()
  (interactive) (projectile-invalidate-cache (projectile-project-root)) (helm-projectile))

(use-package hydra)

(defhydra hydra-leader-menu (:color blue
                             :hint  nil)
    "
^Align^             ^Search^               ^Launch^        ^Navigation^     ^File^
^--^-------------   ^--^-----------------  ^--^----------  ^--^-----------  ^--^--------
_aa_ repeat         _G_  grep helm         _m_  mu4e       _b_ buffers      _n_ rename
_an_ no-repeat      _pp_ pt project dir    _c_  calc       _y_ yank hist    _o_ occur
_a:_ colon          _po_ pt other dir      _d_  dired      _w_ window       _r_ regex
_a=_ equals         ^^                     _tt_ test       _k_ kill buffer  _f_ flycheck
_a,_ comma          ^Project^              _tf_ run-file   _v_ init.el      ^^
_ai_ interactively  ^--^-----------------  _R_  yari       ^^               ^^
^^                  _g_  git               ^^              ^Help^           ^Eval^
^^                  _pi_ invalidate cache  ^^              ^--^-----------  ^--^-----------
^^                  _ps_ switch            ^^              _hh_ descbinds   _e_ eval
^^                  _s_  eshell            ^^              _hm_ discover    _E_ eval print
"
    ;; Align
    ("an" align-no-repeat)
    ("aa" align-repeat)
    ("a:" align-to-colon)
    ("a=" align-to-equals)
    ("a," align-to-comma)
    ("ai" align-interactively)
    ;; File
    ("n" rename-file-and-buffer)
    ("o" helm-occur)
    ("r" helm-regexp)
    ("f" helm-flycheck)
    ;; Search
    ("G" helm-do-grep-recursive)
    ("pp" projectile-pt)
    ("po" pt-regexp)
    ;; Project
    ("g" magit-dispatch-popup)
    ("pi" helm-projectile-invalidate-cache)
    ("ps" helm-projectile-switch-project)
    ("s" eshell-projectile-root)
    ;; Launch
    ("m" mu4e)
    ("c" calc-dispatch)
    ("d" dired)
    ("tt" run-current-test)
    ("tf" (run-current-test nil t))
    ("R" yari)
    ;; Help
    ("hh" helm-descbinds)
    ("hm" discover-my-major)
    ;; Other
    ("e" eval-last-sexp)
    ("E" evil-eval-print-last-sexp)
    ;; Navigation
    ("b" helm-mini)
    ("k" kill-buffer)
    ("y" helm-show-kill-ring)
    ("w" ace-window)
    ("v" (lambda() (interactive) (evil-edit user-init-file)))
    ("zi" text-scale-increase "zoom in" :color pink)
    ("zo" text-scale-decrease "zoom out" :color pink)
)

(define-key evil-normal-state-map (kbd ",") 'hydra-leader-menu/body)
(define-key evil-visual-state-map (kbd ",") 'hydra-leader-menu/body)

;; (use-package evil-leader
;;   :config
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader ",")
;;   (evil-leader/set-key
;;     "e" (kbd "C-x C-e")
;;     "E" 'evil-eval-print-last-sexp
;;     "an" 'align-no-repeat
;;     "aa" 'align-repeat
;;     "a:" 'align-to-colon
;;     "a=" 'align-to-equals
;;     "a," 'align-to-comma
;;     "ai" 'align-interactively
;;     "g" 'magit-dispatch-popup ;; 'magit-status
;;     "G" 'helm-do-grep-recursive
;;     "d" 'dired
;;     "n" 'rename-file-and-buffer
;;     "v" (lambda() (interactive) (evil-edit user-init-file))
;;     "tt" 'run-current-test
;;     "k" 'kill-buffer
;;     "b" 'helm-mini
;;     "p" 'projectile-pt
;;     "P" 'pt-regexp
;;     "o" 'helm-occur
;;     "i" 'helm-projectile-invalidate-cache
;;     "u" 'helm-projectile-switch-project
;;     "f" 'helm-flycheck
;;     "y" 'helm-show-kill-ring
;;     "r" 'helm-regexp
;;     "m" 'mu4e
;;     "w" 'ace-window
;;     "hh" 'helm-descbinds
;;     "s" 'eshell-projectile-root
;;     "R" 'yari
;;     "c" 'calc-dispatch
;;   )
;; )

(defun helm-do-grep-recursive (&optional non-recursive)
  "Like `helm-do-grep', but greps recursively by default."
  (interactive "P")
  (let* ((current-prefix-arg (not non-recursive))
         (helm-current-prefix-arg non-recursive))
    (call-interactively 'helm-do-grep)))

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
  (add-hook 'org-mode-hook (lambda () (setq evil-want-fine-undo 'yes)))

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

  ;; (evil-leader/set-key-for-mode 'org-mode
  ;;   "t"  'org-show-todo-tree
  ;;   "A"  'org-agenda
  ;;   ;; "c"  'org-archive-subtree
  ;;   ;; "l"  'evil-org-open-links
  ;;   ;; "o"  'evil-org-recompute-clocks
  ;; )
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
  ;; tnsedhriaobkgjvmpl
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
  ;; (key-chord-define evil-insert-state-map "ii" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
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
  ;; open new helm split in current window
  ;; (setq helm-split-window-in-side-p nil)
  ;; buffer name length to be length of longest buffer name if nil
  ;; helm-projectile seems to overwrite this for some reason if nil
  (setq helm-buffer-max-length 50)
  (setq helm-display-header-line t)
  :config
  (helm-mode t)
  ;; (helm-adaptive-mode t)
  ;; (helm-autoresize-mode 1)

  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
  ;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  ;; (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

  ;; ;; open helm split at the bottom of a frame
  ;; ;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(rx bos "*helm" (* not-newline) "*" eos)
  ;;                (display-buffer-in-side-window)
  ;;                (inhibit-same-window . t)
  ;;                (window-height . 0.4)))

  ;; Not compatible with above - using shackle instead
  ;; Hydra normal mode in Helm
  (defhydra helm-like-unite ()
    ("m" helm-toggle-visible-mark "mark")
    ("a" helm-toggle-all-marks "(un)mark all")
    ("v" helm-execute-persistent-action)
    ("g" helm-beginning-of-buffer "top")
    ("h" helm-previous-source)
    ("l" helm-next-source)
    ("G" helm-end-of-buffer "bottom")
    ("n" helm-next-line "down")
    ("e" helm-previous-line "up")
    ("q" keyboard-escape-quit "exit" :color blue)
    ("i" nil "insert"))
  ;; (key-chord-define helm-map "ne" 'helm-like-unite/body)
  (define-key helm-map (kbd "C-n") 'helm-like-unite/body)

  ;; tame helm windows by aligning them at the bottom with a ratio of 40%:
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))

  ;; disable popwin-mode in an active Helm session It should be disabled
  ;; otherwise it will conflict with other window opened by Helm persistent
  ;; action, such as *Help* window.
  (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
  (add-hook 'helm-after-initialize-hook (lambda ()
                                          (popwin:display-buffer helm-buffer t)
                                          (popwin-mode -1)))
  ;;  Restore popwin-mode after a Helm session finishes.
  (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))

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

(use-package yari)

(use-package relative-line-numbers
  :diminish ""
  :config
  ;; (global-relative-line-numbers-mode)
  (add-hook  'ruby-mode-hook        'relative-line-numbers-mode)
  (add-hook  'c-mode-common-hook    'relative-line-numbers-mode)
  (add-hook  'python-mode-hook      'relative-line-numbers-mode)
  ;; (add-hook  'shell-mode-hook       'relative-line-numbers-mode)
  (add-hook  'emacs-lisp-mode-hook  'relative-line-numbers-mode)
  (add-hook  'latex-mode-hook       'relative-line-numbers-mode)

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

;; color-identifiers-mode
(add-hook 'ruby-mode-hook        'color-identifiers-mode)
(add-hook 'c-mode-common-hook    'color-identifiers-mode)
(add-hook 'python-mode-hook      'color-identifiers-mode)
(add-hook 'emacs-lisp-mode-hook  'color-identifiers-mode)
(add-hook 'latex-mode-hook       'color-identifiers-mode)
;; (add-hook 'after-init-hook       'global-color-identifiers-mode)
(add-hook 'color-identifiers-mode-hook (lambda () (diminish 'color-identifiers-mode "")))

(use-package flycheck
  :init (global-flycheck-mode)
)

(use-package flymake-ruby
  :init
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)
)

(use-package flymake-haml
  :init
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

(use-package discover-my-major
  :bind (("C-h j" . discover-my-major))
)

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp"
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
  (setenv "PATH"
          (concat "/usr/local/var/rbenv/shims:"
                  "/usr/local/var/rbenv/bin:"
                  (getenv "HOME") "/.rbenv/shims:"
                  (getenv "HOME") "/.rbenv/bin:" (getenv "PATH")))

  (add-to-list 'exec-path "/usr/local/var/rbenv/shims")
  (add-to-list 'exec-path "/usr/local/var/rbenv/bin")
  (add-to-list 'exec-path (concat (getenv "HOME") "/.rbenv/shims"))
  (add-to-list 'exec-path (concat (getenv "HOME") "/.rbenv/bin"))

  (setenv "PAGER" "cat")
  ;; (setq eshell-buffer-shorthand t)

  :config
  (defun eshell-projectile-root ()
    "open eshell in projectile-root"
    (interactive)
    (let ((current-eshell-buffer-name (concat "*eshell:" (projectile-project-name) "*"))
          (current-eshell-buffer     (get-buffer-window
                                      (concat "*eshell:" (projectile-project-name) "*") )))
      (if current-eshell-buffer
          (progn
            (select-window current-eshell-buffer)
            (end-of-buffer)
            (evil-insert-state))
        (progn
          (select-window (split-window-below))
          (evil-window-move-very-top)
          (setenv "PATH" (concat (projectile-project-root) "bin:" (getenv "PATH")))
          (add-to-list 'exec-path (concat (projectile-project-root) "bin"))
          (eshell)
          (rename-buffer current-eshell-buffer-name)
          (insert (concat "cd " (projectile-project-root)))
          (eshell-send-input))
        )
      )
    )

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

           ;; (propertize (concat (shorten-directory (eshell/pwd) 40) " ") 'face
           (propertize (concat (eshell/pwd) " ") 'face
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

  (add-hook 'eshell-mode-hook
    (lambda ()
      (add-to-list 'eshell-visual-commands "ssh")
      (add-to-list 'eshell-visual-commands "tail")
      ))
)

(use-package em-smart
  :init
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
)

(use-package wgrep)
(use-package wgrep-pt
  :config
  (autoload 'wgrep-pt-setup "wgrep-pt")
  (add-hook 'pt-search-mode-hook 'wgrep-pt-setup)
)
(use-package wgrep-ag
  :config
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-search-mode-hook 'wgrep-ag-setup)
)
(use-package wgrep-helm)

;; simple tiling
(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

;; (defadvice swap-with (after advice-for-swap-with activate) (recenter))

(global-set-key (kbd "C-M-n") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-M-e") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-M-H") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-M-L") (lambda () (interactive) (swap-with 'right)))

(global-set-key (kbd "M-N") (lambda () (interactive) (enlarge-window 1)))
(global-set-key (kbd "M-E") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
(global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t)))

;; (global-set-key (kbd "M-n") 'windmove-down)
;; (global-set-key (kbd "M-e") 'windmove-up)
;; (global-set-key (kbd "M-h") 'windmove-left)
;; (global-set-key (kbd "M-l") 'windmove-right)

(defun tmux-navigate (direction)
  (let
      ((cmd (concat "windmove-" direction)))
    (condition-case nil
        (funcall (read cmd))
      (error
       (tmux-command direction)))))
(defun tmux-command (direction)
  (shell-command-to-string
   (concat "tmux select-pane -"
           (tmux-direction direction))))
(defun tmux-direction (direction)
  (upcase
   (substring direction 0 1)))

(global-set-key (kbd "M-n") (lambda () (interactive) (tmux-navigate "down")))
(global-set-key (kbd "M-e") (lambda () (interactive) (tmux-navigate "up")))
(global-set-key (kbd "M-h") (lambda () (interactive) (tmux-navigate "left")))
(global-set-key (kbd "M-l") (lambda () (interactive) (tmux-navigate "right")))

(use-package winner
  :config
  (winner-mode 1))

(provide 'settings)
;;; settings.el ends here
