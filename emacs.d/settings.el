;;; package --- settings.el

;;; Commentary:

;;; Code:

(require 'mouse)
(xterm-mouse-mode t)

(setq echo-keystrokes 0.2)

(setq ring-bell-function (lambda ()))

(setq-default fill-column 80)

(cond ((eq system-type 'cygwin)
       (add-to-list 'default-frame-alist '(font . "PragmataPro-13" )))
      ((eq system-type 'gnu/linux)
       (add-to-list 'default-frame-alist '(font . "PragmataPro-17" )))
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

;; Recent Files minor mode isn't enabled by default
(use-package recentf
  :init
  (setq recentf-max-menu-items 25)
  :config
  (recentf-mode 1)
)

(defun run-current-test (&optional line-no only-run-file)
  (interactive)
  (let ((test-file-window (selected-window))
        (test-file-path   (buffer-file-name (current-buffer)))
        (test-command     (cond (only-run-file "")
                                ((string-match "_spec.rb$" (buffer-file-name (current-buffer)))
                                 "~/.rbenv/shims/ruby ./bin/rspec ")
                                ((string-match ".py$" (buffer-file-name (current-buffer)))
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
  "Move selected lines up one line."
  (interactive)
  (evil-move-lines "up"))

(defun evil-move-lines-down ()
  "Move selected lines down one line."
  (interactive)
  (evil-move-lines "down"))

(defun evil-eval-print-last-sexp ()
  "Eval print when in evil-normal-state."
  (interactive) (forward-char) (previous-line) (eval-print-last-sexp))

(use-package re-builder
  :ensure t
  :init
  (setq reb-re-syntax 'string)
)

(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  ;; (defadvice save-place-find-file-hook (after recenter activate)
  ;;   "Recenter after getting to saved place."
  ;;   (run-with-timer 0 nil (lambda (buf) (dolist (win (get-buffer-window-list buf nil t)) (with-selected-window win (recenter)))) (current-buffer)) )
)

(require 'pos-tip)
;; (use-package pos-tip)

;; (use-package auto-complete
;;   :ensure t
;;   :diminish ""
;;   :config
;;   (setq ac-fuzzy-enable t)
;;   (setq ac-auto-show-menu t)
;;   (setq ac-auto-start t)
;;   (setq ac-quick-help-delay 0.3)
;;   (setq ac-quick-help-height 30)
;;   (setq ac-show-menu-immediately-on-auto-complete t)
;;   (ac-config-default)
;; )
;; (use-package auto-complete-config
;; )

;; set tooltip color
(set-face-attribute 'tooltip nil :background "#303030" :foreground "#c6c6c6")

(use-package company
  :ensure t
  :diminish ""
  :init
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  ;; (set-face-attribute 'company-tooltip nil :background "black" :foreground "gray40")
  ;; (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "gray15")
  ;; (set-face-attribute 'company-preview nil :background "black")
  ;; (set-face-attribute 'company-preview-common nil :inherit 'company-preview :foreground "gray40")
  ;; (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip :background "gray20")
  ;; (set-face-attribute 'company-scrollbar-fg nil :background "gray40")
  :config
  (global-company-mode t)
  ;; (add-hook 'after-init-hook 'global-company-mode)

  ;; Abort company-mode when exiting insert mode
  (defun abort-company-on-insert-state-exit ()
    (company-abort))
  (add-hook 'evil-insert-state-exit-hook 'abort-company-on-insert-state-exit)
)
(if window-system
    ;; doesn't work on the console and overwrites M-h keybinding
    (use-package company-quickhelp
      :init
      :ensure t
      :config
      (company-quickhelp-mode 1)
      )
  )

;; (use-package leuven-theme
;;   :ensure t
;;   :config
;;   (custom-theme-set-faces
;;    'leuven
;;    `(font-lock-keyword-face ((t (:foreground ,(face-foreground font-lock-builtin-face)
;;                                              :background ,(face-background font-lock-builtin-face)))))
;;    `(default ((t (:foreground "#333333" :background "#F5F5F5"))))
;;    `(fringe ((t (:foreground "#8B9B9B" :background "#F5F5F5"))))
;;    )
;; )

(use-package moe-theme
  :ensure t
  :config
  (load-theme 'moe-dark t)
  (custom-theme-set-faces
   'moe-dark

   ;; `(default ((t (:background "#000000"))))

   ;; No Italics (which is sometimes reverse video)
   ;; see: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/2347
   `(font-lock-comment-delimiter-face ((t (:slant normal :foreground "#6c6c6c"))))
   `(font-lock-comment-face           ((t (:slant normal :foreground "#6c6c6c"))))

   `(org-level-1 ((t (:height 1.3 :weight bold :slant normal :foreground "#aa88ff" :underline t)))) ;; purple
   `(org-level-2 ((t (:height 1.0 :weight bold :slant normal :foreground "#88aaff" :underline t)))) ;; blue
   `(org-level-3 ((t (:height 1.0 :weight bold :slant normal :foreground "#88ffff" :underline t)))) ;; cyan
   `(org-level-4 ((t (:height 1.0 :weight bold :slant normal :foreground "#66ffaa" :underline nil)))) ;; sea-green
   `(org-level-5 ((t (:height 1.0 :weight bold :slant normal :foreground "#ffff66" :underline nil)))) ;; yellow
   `(org-level-6 ((t (:height 1.0 :weight bold :slant normal :foreground "#ffaa00" :underline nil)))) ;; orange
   `(org-level-7 ((t (:height 1.0 :weight bold :slant normal :foreground "#ff6666" :underline nil)))) ;; red
   `(org-level-8 ((t (:height 1.0 :weight bold :slant normal :foreground "#ff66aa" :underline nil)))) ;; pink

   `(org-block-begin-line ((t (:foreground "#5a5a5a" :background "#3a3a3a"))))
   ;; `(org-block-end-line   ((t (:foreground "#aa88ff" :background "#aa88ff"))))

   ;; :overline "#A7A7A7" :foreground "#3C3C3C" :background "#F0F0F0"
   ;; :overline "#123555" :foreground "#123555" :background "#E5F4FB"
   ;; :foreground "#005522" :background "#EFFFEF"
   ;; :foreground "#EA6300"
   ;; :foreground "#E3258D"
   ;; :foreground "#0077CC"
   ;; :foreground "#2EAE2C"
   ;; :foreground "#FD8008"

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

   `(mu4e-unread-face ((t (:weight normal :slant normal :foreground "#66ffaa" :underline nil)))) ;; purple

   `(mu4e-contact-face ((t (:weight normal :slant normal :foreground "#88aaff" :underline nil)))) ;; purple
   `(mu4e-header-value-face ((t (:weight normal :slant normal :foreground "#66ffaa" :underline nil)))) ;; purple
   `(mu4e-special-header-value-face ((t (:weight normal :slant normal :foreground "#66ffaa" :underline nil)))) ;; purple

  )
)

;; (if window-system
;;     (load-theme 'base16-atelierdune-dark t)
;;   (load-theme 'base16-shell-dark t))

;; (load-theme 'cyberpunk)
;; (custom-theme-set-faces
;;  'cyberpunk
;;  `(default ((t (:background "#2d2d2d"))))
;;  `(fringe ((t (:background "#2d2d2d")))))

(use-package powerline
  :ensure t
  :init
  (setq powerline-default-separator 'arrow)
  ;; (cond ((eq system-type 'cygwin) (setq powerline-height 26))
  ;;       (t                        (setq powerline-height 28)))
)

(use-package airline-themes
  :load-path "airline-themes"
  ;; :init
  ;; (setq airline-utf-glyph-separator-left      #xe0b0
  ;;       airline-utf-glyph-separator-right     #xe0b2
  ;;       airline-utf-glyph-subseparator-left   #xe0b1
  ;;       airline-utf-glyph-subseparator-right  #xe0b3
  ;;       airline-utf-glyph-branch              #xe0a0
  ;;       airline-utf-glyph-readonly            #xe0a2
  ;;       airline-utf-glyph-linenumber          #xe0a1)
  :config
  ;; (if window-system
  ;;     (load-theme 'airline-base16-gui-dark t)
  ;;   (load-theme 'airline-base16-shell-dark t))
  (if window-system
      (load-theme 'airline-behelit t)
    (load-theme 'airline-behelit t))
  ;; (load-theme 'airline-badwolf)
  ;; (load-theme 'airline-light)
  ;; (load-theme 'airline-papercolor)
)

(use-package rainbow-delimiters
  :ensure t
  :config
  ;; (global-rainbow-delimiters-mode)
  (add-hook 'ruby-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c-mode-common-hook 'rainbow-delimiters-mode)
  (add-hook 'c++-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  ;; (add-hook 'shell-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook 'rainbow-delimiters-mode)
)

;; (use-package guide-key
;;   :diminish ""
;;   :config
;;   (setq guide-key/guide-key-sequence '("C-h" "C-x" "C-c" "C-w" ","))
;;   (setq guide-key/recursive-key-sequence-flag t)
;;   (setq guide-key/popup-window-position 'bottom)
;;   (setq guide-key/idle-delay 1.0)
;;   (guide-key-mode 1)
;; )

;; (use-package guide-key-tip)

(use-package which-key
  :ensure t
  :diminish ""
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom)
)

(use-package ido
  :ensure t
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
  :ensure t
  :config
  (flx-ido-mode 1)
  (setq ido-use-faces nil) ;; disable ido faces to see flx highlights.
)

(use-package undo-tree
  :ensure t
  :diminish ""
)

(use-package evil
  :ensure t
  :init
  ;; (setq evil-search-module 'evil-search)
  (setq x-select-enable-clipboard 1) ;; don't use the clipboard
  (setq evil-want-fine-undo 'no) ;; Make sure undos are done atomically
  (setq evil-want-C-i-jump 'yes)
  (setq evil-want-C-u-scroll 'yes) ;; find some other way to use emacs C-u?
  (setq evil-move-cursor-back nil) ;; don't move back one charachter when exiting insert
  (setq-default evil-symbol-word-search t) ;; make * and # use the whole word

  :config
  (evil-mode 1)

  (define-key  evil-normal-state-map            [escape]  'keyboard-quit)
  (define-key  evil-visual-state-map            [escape]  'keyboard-quit)
  (define-key  evil-emacs-state-map             [escape]  'keyboard-quit)
  (define-key  minibuffer-local-map             [escape]  'minibuffer-keyboard-quit)
  (define-key  minibuffer-local-ns-map          [escape]  'minibuffer-keyboard-quit)
  (define-key  minibuffer-local-completion-map  [escape]  'minibuffer-keyboard-quit)
  (define-key  minibuffer-local-must-match-map  [escape]  'minibuffer-keyboard-quit)
  (define-key  minibuffer-local-isearch-map     [escape]  'minibuffer-keyboard-quit)

  (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

  (define-key evil-insert-state-map (kbd "C-e") 'emmet-expand-line)
  (define-key evil-insert-state-map (kbd "C-t") 'auto-complete)
  (define-key evil-insert-state-map (kbd "C-y") 'counsel-yank-pop)

  (define-key evil-normal-state-map (kbd "C-w N") 'evil-window-move-very-bottom)
  (define-key evil-normal-state-map (kbd "C-w E") 'evil-window-move-very-top)
  (define-key evil-normal-state-map (kbd "C-w H") 'evil-window-move-far-left)
  (define-key evil-normal-state-map (kbd "C-w L") 'evil-window-move-far-right)
  ;; put the current line at the end of the next line
  (define-key evil-normal-state-map (kbd "g j") (lambda() (interactive) (move-line-down) (join-line)))

  (define-key evil-motion-state-map "n" 'evil-next-visual-line)
  (define-key evil-motion-state-map "e" 'evil-previous-visual-line)
  ;; (define-key evil-motion-state-map "k" 'evil-ex-search-next)
  ;; (define-key evil-motion-state-map "K" 'evil-ex-search-previous)
  (define-key evil-motion-state-map "k" 'evil-search-next)
  (define-key evil-motion-state-map "K" 'evil-search-previous)

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

  ;; Center Screen on search hit
  ;; (defadvice evil-ex-search-next (after advice-for-evil-ex-search-next activate)
  ;;   (evil-scroll-line-to-center (line-number-at-pos)))
  ;; (defadvice evil-ex-search-previous (after advice-for-evil-ex-search-previous activate)
  ;;   (evil-scroll-line-to-center (line-number-at-pos)))

  ;; (advice-add 'evil-ex-search-word-forward :after #'recenter)
  ;; (advice-add 'evil-ex-search-next :after #'recenter)
  ;; (advice-add 'evil-ex-search-previous :after #'recenter)

  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'makey-key-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-popup-mode)
  (add-to-list 'evil-normal-state-modes 'package-menu-mode)
  (add-to-list 'evil-motion-state-modes 'flycheck-error-list-mode)
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

(use-package hydra
  :ensure t
)

(defhydra hydra-leader-menu (:color blue :hint nil)
    "
^^-Align---------  ^^-Search------------  ^^-Launch-----  ^^-Navigation--  ^^-File-----
_aa_ repeat        _G_  grep              _o_  org-hydra  _b_ buffers      _n_ rename
_an_ no-repeat     _pp_ pt project dir    _m_  mu4e       _y_ yank hist    _/_ swiper
_a:_ colon         _po_ pt other dir      _c_  calc       _k_ kilc buffer  _r_ regentf
_a=_ equals        ^^                     _d_  find-file  _v_ init.el      _f_ flycheck
_a,_ comma         ^^                     _tt_ test       ^^               ^^
_ai_ interactive   ^^-Project-----------  _tf_ run-file   _ww_ ace-window  ^^
^^                 _g_  git               _R_  yari       _wu_ win-undo    ^^
^-Help-^-------    _pi_ invalidate cache  _lt_ load-theme _wr_ win-redo    ^^-Eval-------
_hh_ descbinds     _ps_ switch            _lp_ list pckgs _zi_ zoom-in     _e_ eval
_hm_ discover      _s_  eshell            ^^              _zo_ zoom-out    _E_ eval print"
    ;; Align
    ("an" align-no-repeat)
    ("aa" align-repeat)
    ("a:" align-to-colon)
    ("a=" align-to-equals)
    ("a," align-to-comma)
    ("ai" align-interactively)
    ;; File
    ("n" rename-file-and-buffer)
    ("/" swiper)
    ("r" ivy-recentf)
    ("f" flycheck-list-errors)
    ;; Search
    ("G" counsel-git-grep)
    ("pp" projectile-pt)
    ("po" pt-regexp)
    ;; Project
    ("g" magit-dispatch-popup)
    ("pi" projectile-invalidate-cache)
    ("ps" ivy-switch-project)
    ("s" eshell-projectile-root)
    ;; Launch
    ("m" mu4e)
    ("c" calc-dispatch)
    ("d" counsel-find-file)
    ("tt" run-current-test)
    ("tf" (run-current-test nil t))
    ("R" yari)
    ("lt" load-theme)
    ("lp" package-list-packages)
    ;; Help
    ("hh" amd-display-binds)
    ("hm" discover-my-major)
    ;; Other
    ("e" eval-defun)
    ("E" amd-edebug-eval-defun)
    ;; Navigation
    ("b" ivy-switch-buffer)
    ("B" ibuffer)
    ("k" kill-buffer)
    ("y" counsel-yank-pop)
    ("ww" ace-window)
    ("wu" winner-undo)
    ("wr" winner-redo)
    ("v" (lambda() (interactive) (evil-edit user-init-file)))
    ("zi" text-scale-increase :color pink)
    ("zo" text-scale-decrease :color pink)
    ("o" hydra-org-menu/body)
)

(define-key evil-normal-state-map (kbd ",") 'hydra-leader-menu/body)
(define-key evil-motion-state-map (kbd ",") 'hydra-leader-menu/body)
(define-key evil-visual-state-map (kbd ",") 'hydra-leader-menu/body)

(defhydra hydra-org-menu (:color blue :hint nil)
    "
^^-Todos-------  ^^^^-MetaShift--^^  ^^-Export--
_a_  agenda      ^ ^ _n_ ^ ^    ↑    _x_  export
_t_  todo-tree   _h_ _e_ _l_  ← ↓ →
_o_  open todos
"
  ("t" org-show-todo-tree)
  ("a" org-agenda)
  ("o" (lambda() (interactive) (find-file "~/Dropbox/org/todo.org")))
  ("x" org-export-dispatch)
  ("n" org-shiftmetadown  :color pink)
  ("e" org-shiftmetaup    :color pink)
  ("h" org-shiftmetaleft  :color pink)
  ("l" org-shiftmetaright :color pink)
)

(use-package org
  :ensure t
  :defer t
  :init
  (setq org-default-notes-file "~/Dropbox/org/todo.org")
  :config

  ;; prettify-symbols-mode only operates on strings
  ;; (add-hook 'org-mode-hook 'prettify-symbols-mode)
  ;; (add-hook 'org-mode-hook (lambda () (push '((regexp-quote "^**") . " *") prettify-symbols-alist)))
  (add-hook 'org-mode-hook 'org-bullets-mode)

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
    "o" '(lambda () (interactive) (evil-org-eol-call 'clever-insert-item))
    "O" '(lambda () (interactive) (evil-org-eol-call 'org-insert-heading))

    "^" 'org-beginning-of-line
    "$" 'org-end-of-line

    "<" 'org-shiftmetaleft
    ">" 'org-shiftmetaright

    "-" 'org-cycle-list-bullet
    (kbd "TAB") 'org-cycle
  )


  (mapc
   (lambda (state)
     (evil-define-key state evil-org-mode-map
       ;; rebind some existing org-mode maps
       (kbd "M-e") (lambda () (interactive) (tmux-navigate "up"))   ;; was org-forward-sentence
       (kbd "M-h") (lambda () (interactive) (tmux-navigate "left")) ;; was org-mark-element

       ;; (kbd "M-l") 'org-metaright
       ;; (kbd "M-h") 'org-metaleft
       ;; (kbd "M-e") 'org-metaup
       ;; (kbd "M-n") 'org-metadown

       ;; (kbd "M-L") 'org-shiftmetaright
       ;; (kbd "M-H") 'org-shiftmetaleft
       ;; (kbd "M-E") 'org-shiftmetaup
       ;; (kbd "M-N") 'org-shiftmetadown

     ))
   '(normal insert))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (python . t)
     (ruby . t)
     (ditaa . t)
     (sqlite . t)
     (perl . t)
     ))

  (setq org-babel-ruby-command "~/.rbenv/shims/ruby")
)

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1)
  (add-hook 'web-mode-hook (lambda ()
                             (add-to-list 'evil-surround-pairs-alist '(?h . ("{{ " . " }}"))  )
                             (add-to-list 'evil-surround-pairs-alist '(?= . ("<%= " . " %>")) )
                             (add-to-list 'evil-surround-pairs-alist '(?- . ("<% "  . " %>")) )))
)

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-commentary
  :ensure t
  :diminish ""
  :config
  (evil-commentary-mode)
)

(use-package evil-jumper
  :ensure t
  :config
  (global-evil-jumper-mode)
  (advice-add 'evil-jumper/forward :after #'recenter)
  (advice-add 'evil-jumper/back :after #'recenter)
)

(use-package avy
  :ensure t
  :config
  ;; tnsedhriaobkgjvmpl
  (setq avy-keys '(?t ?n ?s ?e ?d ?h ?r ?i ?a ?o ?b ?k ?g ?j ?v ?m ?p ?l))
  (setq avy-background t)
  (define-key evil-normal-state-map (kbd "t") 'avy-goto-char-2)
)

(use-package ace-window
  :ensure t
  :config
  ;; (setq aw-keys '(?t ?n ?s ?e ?d ?h ?r ?i ?a ?o ?b ?k ?g ?j ?v ?m ?p ?l))
  ;; show the window letter in the modeline
  ;; (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
  ;; (ace-window-display-mode t)

  (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)

  (setq aw-keys   '(?n ?e ?i ?l ?u ?y)
        aw-dispatch-always t
        aw-swap-invert t
        aw-dispatch-alist
        '((?c aw-delete-window     "Ace - Delete Window")
          (?r aw-swap-window       "Ace - Swap Window")
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?s aw-split-window-horz "Ace - Split Horz Window")
          (?o delete-other-windows "Ace - Maximize Window")
          (?p aw-flip-window)
          (?= balance-windows)
          ;; (?u winner-undo)
          ;; (?r winner-redo)
          )
       )

  (when (package-installed-p 'hydra)
    (defhydra hydra-window-size (:color red)
      "Windows size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("n" shrink-window "shrink vertical")
      ("e" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))

    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("n" make-frame "new frame")
      ("c" delete-frame "delete frame"))

    ;; (defhydra hydra-window-scroll (:color red)
    ;;   "Scroll other window"
    ;;   ("n" joe-scroll-other-window "scroll")
    ;;   ("p" joe-scroll-other-window-down "scroll down"))

    ;; (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
    (add-to-list 'aw-dispatch-alist '(?+ hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?f hydra-window-frame/body) t)
  )
)

(use-package ace-link
  :ensure t
  :config
  ;; (ace-link-setup-default)
  (eval-after-load "eww"
    `(progn
       (define-key eww-link-keymap (kbd "f") 'ace-link-eww)
       (define-key eww-mode-map (kbd "f") 'ace-link-eww)))
)

;; ;; key-chord http://www.emacswiki.org/emacs/key-chord.el
;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (setq key-chord-two-keys-delay 0.2)
;;   ;; (key-chord-define evil-insert-state-map "jj" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
;;   ;; (key-chord-define evil-insert-state-map "ii" (lambda() (interactive) (evil-normal-state) (evil-forward-char)))
;;   (key-chord-mode 1)
;; )

;; Projectile https://github.com/bbatsov/projectile
(use-package projectile
  :ensure t
  :defer 2
  :init
  ;; (setq projectile-completion-system 'helm)
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-switch-project-action 'helm-projectile)
  (setq projectile-switch-project-action 'projectile-find-file)
  (setq projectile-globally-ignored-directories '("vendor/ruby"))
  (setq projectile-require-project-root nil) ;; use projectile everywhere (no .projectile file needed)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  :config
  (projectile-global-mode t)
)

;; (use-package helm
;;   :ensure t
;;   :diminish ""
;;   :bind (("M-x" . helm-M-x))
;;   :init
;;   (setq
;;    helm-mode-fuzzy-match t
;;    helm-completion-in-region-fuzzy-match t
;;    helm-recentf-fuzzy-match t
;;    helm-buffers-fuzzy-matching t
;;    helm-locate-fuzzy-match t
;;    helm-M-x-fuzzy-match t
;;    helm-semantic-fuzzy-match t
;;    helm-imenu-fuzzy-match t
;;    helm-apropos-fuzzy-match t
;;    helm-lisp-fuzzy-completion t)
;;   ;; open new helm split in current window
;;   ;; (setq helm-split-window-in-side-p nil)
;;   ;; buffer name length to be length of longest buffer name if nil
;;   ;; helm-projectile seems to overwrite this for some reason if nil
;;   (setq helm-buffer-max-length 50)
;;   (setq helm-display-header-line t)
;;   :config
;;   (helm-mode t)
;;   ;; (helm-adaptive-mode t)
;;   ;; (helm-autoresize-mode 1)

;;   (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
;;   (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;;   (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

;;   ;; ;; open helm split at the bottom of a frame
;;   ;; ;; https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
;;   ;; (add-to-list 'display-buffer-alist
;;   ;;              `(,(rx bos "*helm" (* not-newline) "*" eos)
;;   ;;                (display-buffer-in-side-window)
;;   ;;                (inhibit-same-window . t)
;;   ;;                (window-height . 0.4)))

;;   ;; Not compatible with above - using shackle instead
;;   ;; Hydra normal mode in Helm
;;   (defhydra helm-like-unite (:columns 6)
;;     "Normal Mode"
;;     ("m" helm-toggle-visible-mark "mark")
;;     ("M" helm-toggle-all-marks "(un)mark all")
;;     ("p" helm-execute-persistent-action "preview")
;;     ("gg" helm-beginning-of-buffer "top")
;;     ("G" helm-end-of-buffer "bottom")
;;     ("k" helm-buffer-run-kill-persistent "kill")
;;     ("h" helm-previous-source "next source")
;;     ("l" helm-next-source "prev source")
;;     ("n" helm-next-line "down")
;;     ("e" helm-previous-line "up")
;;     ("q" keyboard-escape-quit "exit" :color blue)
;;     ("i" nil "insert"))
;;   ;; (key-chord-define helm-map "ne" 'helm-like-unite/body)
;;   (define-key helm-map (kbd "C-n") 'helm-like-unite/body)

;;   ;; tame helm windows by aligning them at the bottom with a ratio of 40%:
;;   (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))

;;   ;; ;; disable popwin-mode in an active Helm session It should be disabled
;;   ;; ;; otherwise it will conflict with other window opened by Helm persistent
;;   ;; ;; action, such as *Help* window.
;;   ;; (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
;;   ;; (add-hook 'helm-after-initialize-hook (lambda ()
;;   ;;                                         (popwin:display-buffer helm-buffer t)
;;   ;;                                         (popwin-mode -1)))
;;   ;; ;;  Restore popwin-mode after a Helm session finishes.
;;   ;; (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))
;; )

;; (use-package helm-config
;;   :defer t
;; )
;; (use-package helm-projectile
;;   :ensure t
;;   ;; :defer t
;;   :config
;;   (helm-projectile-on)
;; )
;; (use-package helm-descbinds
;;   :ensure t
;;   :defer t
;;   :bind (("C-h j" . helm-descbinds))
;;   :config
;;   (helm-descbinds-mode)
;; )
;; (use-package helm-flx
;;   :ensure t
;;   :defer t
;;   :config
;;   (helm-flx-mode +1)
;; )
;; (use-package helm-fuzzier
;;   :ensure t
;;   :defer t
;;   :config
;;   (helm-fuzzier-mode 1)
;; )

;; (defun helm-projectile-invalidate-cache ()
;;   (interactive) (projectile-invalidate-cache (projectile-project-root)) (helm-projectile))

;; (defun helm-do-grep-recursive (&optional non-recursive)
;;   "Like `helm-do-grep', but greps recursively by default."
;;   (interactive "P")
;;   (let* ((current-prefix-arg (not non-recursive))
;;          (helm-current-prefix-arg non-recursive))
;;     (call-interactively 'helm-do-grep)))


(use-package shackle
  :ensure t
  :diminish ""
  :config
  (shackle-mode))

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
)

;; Web Settings
(use-package web-mode
  :ensure t
  :init
  (setq web-mode-engines-alist '(("liquid" . "\\.html\\'")))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))
)

;; Python Settings
(use-package jedi
  :ensure t
  :defer t
  :config
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t)
)

(use-package js2-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; (add-hook 'js-mode-hook 'js2-minor-mode)
)

;; (use-package ac-js2
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'js2-mode-hook 'ac-js2-mode)
;; )

;; Ruby Settings
(use-package robe
  :ensure t
  :defer t
  :init
  (setq ruby-deep-indent-paren nil)
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

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq evil-shift-width python-indent))))
(add-hook 'ruby-mode-hook
          (function (lambda ()
                      (setq evil-shift-width ruby-indent-level))))

(use-package yari
  :ensure t
)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

(use-package relative-line-numbers
  :ensure t
  :diminish ""
  :config
  ;; (global-relative-line-numbers-mode)
  (add-hook  'ruby-mode-hook        'relative-line-numbers-mode)
  (add-hook  'c-mode-common-hook    'relative-line-numbers-mode)
  (add-hook  'c++-mode-hook         'relative-line-numbers-mode)
  (add-hook  'python-mode-hook      'relative-line-numbers-mode)
  ;; (add-hook  'shell-mode-hook       'relative-line-numbers-mode)
  (add-hook  'emacs-lisp-mode-hook  'relative-line-numbers-mode)
  (add-hook  'latex-mode-hook       'relative-line-numbers-mode)
  (add-hook  'js2-mode-hook         'relative-line-numbers-mode)

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
(add-hook 'c++-mode-hook         'color-identifiers-mode)
(add-hook 'python-mode-hook      'color-identifiers-mode)
(add-hook 'emacs-lisp-mode-hook  'color-identifiers-mode)
(add-hook 'latex-mode-hook       'color-identifiers-mode)
(add-hook 'js2-mode-hook         'color-identifiers-mode)
;; (add-hook 'after-init-hook       'global-color-identifiers-mode)
(add-hook 'color-identifiers-mode-hook (lambda () (diminish 'color-identifiers-mode "")))

(use-package flycheck
  :ensure t
  :diminish ""
  :config
  (global-flycheck-mode)
)

(use-package flymake-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'flymake-ruby-load)
)

(use-package flymake-haml
  :ensure t
  :init
  (add-hook 'haml-mode-hook 'flymake-haml-load)
)

(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
)

(use-package which-function
  :config
  (which-function-mode t)
)

(use-package dired
  :defer t
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  :config
  (define-key dired-mode-map (kbd "C-c C-w") 'dired-toggle-read-only)
  (define-key dired-mode-map (kbd ",") 'hydra-leader-menu/body)
  (define-key dired-mode-map (kbd "f") 'dired-find-file)
  (define-key dired-mode-map (kbd "e") 'dired-previous-line) ;; colemak

  (defadvice dired-toggle-read-only (after advice-for-dired-toggle-read-only activate)
    (evil-normal-state))
)

(use-package dired-x
  :init
  (cond ((eq system-type 'darwin)
         (setq insert-directory-program "/usr/local/bin/gls"))
        (t
         (setq insert-directory-program "ls")))
)

(use-package dired-subtree
  :load-path "dired-hacks"
  :config
  (define-key dired-mode-map (kbd "z") 'dired-subtree-toggle)
)

(use-package discover
  :ensure t
  :config
  (global-discover-mode 1)
)

(use-package discover-my-major
  :ensure t
  ;; :bind (("C-h j" . discover-my-major))
)

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e"
  :init
  (setq mu4e-mu-binary "/usr/local/bin/mu")
  (setq mu4e-confirm-quit nil)
  :config
  (mapc (lambda (current-mode-map-name)
          (define-key current-mode-map-name (kbd ",") 'hydra-leader-menu/body))
        '(mu4e-headers-mode-map
          mu4e-view-mode-map
          mu4e-main-mode-map))

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

  (defun open-docx-attachment-in-emacs (msg attachnum)
    "Count the number of lines in an attachment."
    (mu4e-view-pipe-attachment msg attachnum "cat > ~/Downloads/attachment.docx && pandoc -f docx -t org ~/Downloads/attachment.docx"))

  (defun open-xlsx-attachment-in-emacs (msg attachnum)
    "Count the number of lines in an attachment."
    (mu4e-view-pipe-attachment msg attachnum "cat > ~/Downloads/attachment.xlsx && xlsx2csv ~/Downloads/attachment.xlsx"))

  ;; defining 'n' as the shortcut
  (add-to-list 'mu4e-view-attachment-actions
    '("cview-docx" . open-docx-attachment-in-emacs) t)
  (add-to-list 'mu4e-view-attachment-actions
    '("xview-xlsx" . open-xlsx-attachment-in-emacs) t)
)

(use-package smtpmail
  :ensure t
  :config
  (setq smtpmail-queue-mail t)
  (setq smtpmail-queue-dir  "~/Mail/queue/cur")

  (setq message-kill-buffer-on-exit t)
)

(use-package eshell
  :ensure t
  :defer t
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

  ;; :config
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

  ;; ;; Turn on helm completion and history
  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (define-key eshell-mode-map
  ;;               [remap eshell-pcomplete]
  ;;               'helm-esh-pcomplete)))

  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (define-key eshell-mode-map
  ;;               (kbd "M-p")
  ;;               'helm-eshell-history)))

  (defun eshell/x ()
    "Closes the EShell session and gets rid of the EShell window."
    (interactive)
    (let ((current-eshell-buffer      (get-buffer-window
                                      (concat "*eshell:" (projectile-project-name) "*"))))
      (when current-eshell-buffer
        (progn
          (select-window current-eshell-buffer)
          (kill-buffer)
          (delete-window))
      )
    )
  )

  (add-hook 'eshell-mode-hook
    (lambda ()
      (add-to-list 'eshell-visual-commands "ssh")
      (add-to-list 'eshell-visual-commands "tail")))
)

(use-package em-smart
  :defer t
  :init
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands t)
  (setq eshell-smart-space-goes-to-end t)
)

(use-package wgrep
  :ensure t
)
(use-package wgrep-pt
  :ensure t
  :config
  (autoload 'wgrep-pt-setup "wgrep-pt")
  (add-hook 'pt-search-mode-hook 'wgrep-pt-setup)
)
(use-package wgrep-ag
  :ensure t
  :config
  (autoload 'wgrep-ag-setup "wgrep-ag")
  (add-hook 'ag-search-mode-hook 'wgrep-ag-setup)
)
;; (use-package wgrep-helm
;;   :ensure t
;; )

;; simple tiling
(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when (and other-window (not (eq other-window (minibuffer-window))))
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

(global-set-key (kbd "C-M-n") (lambda () (interactive) (swap-with 'down) (windmove-down)))
(global-set-key (kbd "C-M-e") (lambda () (interactive) (swap-with 'up) (windmove-up)))
(global-set-key (kbd "C-M-h") (lambda () (interactive) (swap-with 'left) (windmove-left)))
(global-set-key (kbd "C-M-l") (lambda () (interactive) (swap-with 'right) (windmove-right)))

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

;; (add-to-list 'prettify-symbols-alist '(">=" . ?))

(add-hook 'ruby-mode-hook 'prettify-symbols-mode)
(add-hook 'ruby-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)))

;; (add-hook 'emacs-lisp-mode-hook
;;   (lambda () (push '("<=" . ?≤) prettify-symbols-alist)))

(use-package ibuffer-vc
  :init
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
  :config
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic))))
)

(use-package smex
;;   :config
;;   (smex-initialize)
;;   :bind (("M-x" . smex)
;;          ("M-X" . smex-major-mode-commands)
;;          ("C-c C-c M-x" . execute-extended-command))
)

(use-package swiper
  :ensure t
  :diminish ivy-mode
  :init
  (setq ivy-display-style 'fancy)
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-height 10)
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  ;; (advice-add 'swiper :after #'recenter)
  (advice-add 'swiper :after #'amd-update-evil-search)
)

(defun amd-update-evil-search ()
  "Update evil search module highlight."
  (recenter)
  ;; (- (point) 1)
  ;; (evil-ex-find-next (evil-ex-make-search-pattern (ivy--regex ivy-text)) 'backward t) ;; regex for user typed string
)

;; (use-package ivy
;;   :diminish ""
;; )

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x))
  :init
)

(defvar ivy-switch-project-map (make-sparse-keymap))

(defun ivy-switch-project ()
  (interactive)
  (let ((this-command 'ivy-switch-project))
    (ivy-read
     "Switch to project: "
     (if (projectile-project-p)
         (cons (abbreviate-file-name (projectile-project-root))
               (projectile-relevant-known-projects))
       projectile-known-projects)
     :action #'projectile-switch-project-by-name
     :keymap ivy-switch-project-map)))

;; (global-set-key (kbd "C-c m") 'ivy-switch-project)

(ivy-set-actions
 'ivy-switch-project
 '(("k"
    (lambda (x)
      (setq projectile-known-projects
            (--reject (string= x it) projectile-known-projects))
      (projectile-merge-known-projects)
      ;; (projectile-remove-known-project x)
      (ivy--reset-state ivy-last))
    "remove project")
   ("d"
    (lambda (x)
      (dired x)
      )
    "dired")))

(defhydra hydra-ivy-switch-project (:color pink)
  "Buffer Actions"
  ("k" amd-ivy-remove-project)
  ("gg" ivy-beginning-of-buffer)
  ("n" ivy-next-line)
  ("e" ivy-previous-line)
  ("G" ivy-end-of-buffer)
  ("o" keyboard-escape-quit :exit t)
  ("C-g" keyboard-escape-quit :exit t)
  ("i" nil)
)

(define-key ivy-switch-project-map (kbd "C-b") 'hydra-ivy-switch-project/body)

(defun amd-ivy-remove-project ()
  (interactive)
  (setq projectile-known-projects
        (--reject (string= ivy--current it) projectile-known-projects))
  (projectile-merge-known-projects)
  (ivy--reset-state ivy-last)
)

(defun amd-ivy-kill-buffer ()
  (interactive)
  (kill-buffer ivy--current)
  (ivy--reset-state ivy-last)
)

(defhydra hydra-counsel-switch-buffer (:color pink)
  "Buffer Actions"
  ("k" amd-ivy-kill-buffer)
  ("gg" ivy-beginning-of-buffer)
  ("n" ivy-next-line)
  ("e" ivy-previous-line)
  ("G" ivy-end-of-buffer)
  ("o" keyboard-escape-quit :exit t)
  ("C-g" keyboard-escape-quit :exit t)
  ("i" nil)
)

(define-key ivy-switch-buffer-map (kbd "C-b") 'hydra-counsel-switch-buffer/body)

(defun counsel-pt (&optional initial-input initial-directory)
  "Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (setq counsel--git-grep-dir (or initial-directory default-directory))
  (ivy-read "pt: " 'counsel-pt-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))))


(defun amd-edebug-eval-defun ()
  "Run eval-defun with C-u."
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-inter/actively 'eval-defun)))

(defun amd-display-binds ()
  (interactive)
  (ivy-read "keys: "
  (mapcar
   (lambda (keys) (cons
                   (format "%16s  %s" (car keys) (cdr keys))
                   (car keys)))
     (which-key--get-current-bindings))
  :action (lambda (key) (message key))
  )
)


(defun counsel-pt-function (string &optional _pred &rest _unused)
  "Grep in the current directory for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory counsel--git-grep-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (counsel--async-command
       (format "pt -e --nocolor --nogroup -- %S" regex))
      nil)))

(provide 'settings)
;;; settings.el ends here
