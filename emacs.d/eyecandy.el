;;; eyecandy.el -- Theming configuration for my Emacs

;;; Commentary:
;;; powerline and rainbow-delimiters

;;; Code:

;; (defface powerline-custom1 '((t (:background "#EEAD0E" :foreground "black" :weight bold)))
;;   "Custom face for bright sections"
;;   :group 'powerline)

;; (defface powerline-custom2 '((t (:foreground "#EEAD0E" :weight bold)))
;;   "Custom face for text"
;;   :group 'powerline)

; (require 'anzu)
; (defun my/anzu-update-func (here total)
;   "Customizing how anzu displays HERE & TOTAL on the mode line."
;   (propertize (format " <%d/%d>" here total)
;               'face 'powerline-custom1))
; (setq anzu-mode-line-update-function 'my/anzu-update-func)

;; Badwolf
;; let s:N1 = [ '#141413' , '#aeee00' , 232 , 154 ] " blackestgravel & lime
;; let s:N2 = [ '#f4cf86' , '#45413b' , 222 , 238 ] " dirtyblonde    & deepgravel
;; let s:N3 = [ '#8cffba' , '#242321' , 121 , 235 ] " saltwatertaffy & darkgravel
;; let s:N4 = [ '#666462' , 241 ]                   " mediumgravel

;; let s:I1 = [ '#141413' , '#0a9dff' , 232 , 39  ] " blackestgravel & tardis
;; let s:I2 = [ '#f4cf86' , '#005fff' , 222 , 27  ] " dirtyblonde    & facebook
;; let s:I3 = [ '#0a9dff' , '#242321' , 39  , 235 ] " tardis         & darkgravel

;; let s:V1 = [ '#141413' , '#ffa724' , 232 , 214 ] " blackestgravel & orange
;; let s:V2 = [ '#000000' , '#fade3e' , 16  , 221 ] " coal           & dalespale
;; let s:V3 = [ '#000000' , '#b88853' , 16  , 137 ] " coal           & toffee
;; let s:V4 = [ '#c7915b' , 173 ]                   " coffee

(defface outer-normal  '((t (:foreground "#141413" :background "#aeee00" :weight bold))) "Outer Normal Face"  :group 'powerline)
(defface inner-normal  '((t (:foreground "#f4cf86" :background "#45413b" :weight bold))) "Inner Normal Face"  :group 'powerline)
(defface center-normal '((t (:foreground "#8cffba" :background "#242321" :weight bold))) "Center Normal Face" :group 'powerline)

(defface outer-insert  '((t (:foreground "#141413" :background "#0a9dff" :weight bold))) "Outer Insert Face"  :group 'powerline)
(defface inner-insert  '((t (:foreground "#f4cf86" :background "#005faf" :weight bold))) "Inner Insert Face"  :group 'powerline)
(defface center-insert '((t (:foreground "#0a9dff" :background "#242321" :weight bold))) "Center Insert Face" :group 'powerline)

(defface outer-visual  '((t (:foreground "#141413" :background "#ffa724" :weight bold))) "Outer Visual Face"  :group 'powerline)
(defface inner-visual  '((t (:foreground "#000000" :background "#fade3e" :weight bold))) "Inner Visual Face"  :group 'powerline)
(defface center-visual '((t (:foreground "#000000" :background "#b88853" :weight bold))) "Center Visual Face" :group 'powerline)

(set-face-attribute 'mode-line nil :foreground "#fff" :background "#FF0066")

(defun powerline-spacemacs-imitation-theme ()
  "An attempt to imitate the spacemacs powerline theme."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (face3 (if active 'powerline-custom1 mode-line))
                          (face4 (if active 'powerline-custom2 mode-line))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          ;; Left Hand Side
                          ;; (string= (symbol-name evil-state) "normal")
                          ;; (eq evil-state (intern "normal"))
                          (outer-face (cond ((eq evil-state (intern "normal"))
                                             (if (powerline-selected-window-active) 'outer-normal 'powerline-inactive1))
                                            ((eq evil-state (intern "insert"))
                                             (if (powerline-selected-window-active) 'outer-insert 'powerline-inactive1))
                                            ((eq evil-state (intern "visual"))
                                             (if (powerline-selected-window-active) 'outer-visual 'powerline-inactive1))
                                            (t
                                             (if (powerline-selected-window-active) 'outer-normal 'powerline-inactive1))))
                          (inner-face (cond ((eq evil-state (intern "normal"))
                                             (if (powerline-selected-window-active) 'inner-normal 'powerline-inactive2))
                                            ((eq evil-state (intern "insert"))
                                             (if (powerline-selected-window-active) 'inner-insert 'powerline-inactive2))
                                            ((eq evil-state (intern "visual"))
                                             (if (powerline-selected-window-active) 'inner-visual 'powerline-inactive2))
                                            (t
                                             (if (powerline-selected-window-active) 'inner-normal 'powerline-inactive2))))
                          (center-face (cond ((eq evil-state (intern "normal"))
                                              (if (powerline-selected-window-active) 'center-normal 'powerline-inactive2))
                                             ((eq evil-state (intern "insert"))
                                              (if (powerline-selected-window-active) 'center-insert 'powerline-inactive2))
                                             ((eq evil-state (intern "visual"))
                                              (if (powerline-selected-window-active) 'center-visual 'powerline-inactive2))
                                             (t
                                              (if (powerline-selected-window-active) 'center-normal 'powerline-inactive2))))

                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (concat " " (powerline-evil-tag) " ") outer-face)))
                                     (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (funcall separator-left outer-face inner-face)))

                                     ;; ;; Separator >
                                     ;; (powerline-raw (char-to-string #x2b81) inner-face 'l)

                                     ;; Git Branch
                                     (powerline-vc inner-face)

                                     ;; Modified string?
                                     (powerline-raw "%*" inner-face 'l)

                                     ;; Separator >
                                     (powerline-raw " " inner-face)
                                     (funcall separator-left inner-face center-face)

                                     ;; Directory
                                     (powerline-raw (shorten-directory default-directory 20)
                                                    center-face 'l)

                                     ;; Buffer ID
                                     (powerline-buffer-id center-face)

                                     ;; Current Function (which-function-mode)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format 'l nil))

                                     ;; ;; Separator >
                                     ;; (powerline-raw " " center-face)
                                     ;; (funcall separator-left mode-line face1)

                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))

                                     ;; ;; Separator <
                                     ;; (powerline-raw " " face1)
                                     ;; (funcall separator-right face1 face2)
                                   ))

                          ;; Right Hand Side
                          (rhs (list (powerline-raw global-mode-string face2 'r)

                                     ;; ;; Separator <
                                     ;; (powerline-raw (char-to-string #x2b83) center-face 'l)

                                     ;; Minor Modes
                                     (powerline-minor-modes center-face 'l)
                                     ;; (powerline-narrow center-face 'l)

                                     ;; Separator <
                                     (powerline-raw (char-to-string #x2b83) center-face 'l)

                                     ;; Major Mode
                                     (powerline-major-mode center-face 'l)
                                     (powerline-process center-face)

                                     ;; Separator <
                                     (powerline-raw " " center-face)
                                     (funcall separator-right center-face inner-face)

                                     ;; Buffer Size
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size inner-face 'l))

                                     ;; Mule Info
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info inner-face 'l))

                                     (powerline-raw " " inner-face)

                                     ;; Separator <
                                     (funcall separator-right inner-face outer-face)

                                     ;; LN charachter?
                                     (unless window-system
                                       (powerline-raw (char-to-string #x2b61) outer-face 'l))
                                       ;; (powerline-raw (char-to-string #xe0a1) outer-face 'l))

                                     ;; Current Line
                                     (powerline-raw "%4l" outer-face 'l)
                                     (powerline-raw ":" outer-face 'l)
                                     ;; Current Column
                                     (powerline-raw "%3c" outer-face 'r)

                                     ;; % location in file
                                     (powerline-raw "%6p" outer-face 'r))))

                     ;; Combine Left and Right Hand Sides
                     (concat (powerline-render lhs)
                             (powerline-fill center-face (powerline-width rhs))
                             (powerline-render rhs)))))))

(cond ((eq (frame-parameter nil 'background-mode) 'light)
       (set-face-attribute 'mode-line-buffer-id nil :background nil :foreground "#1c1c1c")
       (set-face-attribute 'mode-line-inactive nil :background "#b2b2b2" :foreground "#ffffff")
       (set-face-attribute 'powerline-active2 nil :background "#585858" :foreground "#ffffff")
       (set-face-attribute 'powerline-inactive1 nil :background "#c6c6c6" :foreground "#585858")
       (set-face-attribute 'powerline-inactive2 nil :background "#e4e4e4" :foreground "#585858")
       (cond ((eq moe-theme-mode-line-color 'blue)
              (set-face-attribute 'mode-line nil :background "#5fafd7" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#afd7ff" :foreground "#005faf"))
             ((eq moe-theme-mode-line-color 'green)
              (set-face-attribute 'mode-line nil :background "#a1db00" :foreground "#005f00")
              (set-face-attribute 'powerline-active1 nil :background "#d7ff87" :foreground "#008700"))
             ((eq moe-theme-mode-line-color 'orange)
              (set-face-attribute 'mode-line nil :background "#ff8700" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#ffd787" :foreground "#d75f00"))
             ((eq moe-theme-mode-line-color 'magenta)
              (set-face-attribute 'mode-line nil :background "#ff4ea3" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#ffafff" :foreground "#ff1f8b"))
             ((eq moe-theme-mode-line-color 'yellow)
              (set-face-attribute 'mode-line nil :background "#fce94f" :foreground "#875f00")
              (set-face-attribute 'powerline-active1 nil :background "#ffff87" :foreground "#875f00"))
             ((eq moe-theme-mode-line-color 'purple)
              (set-face-attribute 'mode-line nil :background "#af5fd7" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#e6a8df" :foreground "#6c0099"))
             ((eq moe-theme-mode-line-color 'red)
              (set-face-attribute 'mode-line nil :background "#ff4b4b" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#ffafaf" :foreground "#cc0000"))
             ((eq moe-theme-mode-line-color 'cyan)
              (set-face-attribute 'mode-line nil :background "#5faf87" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#87d7af" :foreground "#005f5f"))
             ((eq moe-theme-mode-line-color 'w/b)
              (set-face-attribute 'mode-line nil :background "#1c1c1c" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#bcbcbc" :foreground "#3a3a3a")
              (set-face-attribute 'mode-line-buffer-id nil :background nil :foreground "#ffffff"))))
      ((eq (frame-parameter nil 'background-mode) 'dark)
       (set-face-attribute 'mode-line-buffer-id nil :background nil :foreground "#080808")
       (set-face-attribute 'mode-line-inactive nil :background "#4e4e4e" :foreground "#9e9e9e")
       (set-face-attribute 'powerline-active2 nil :background "#ffffff" :foreground "#3a3a3a")
       (set-face-attribute 'powerline-inactive1 nil :background "#626262" :foreground "#eeeeee")
       (set-face-attribute 'powerline-inactive2 nil :background "#767676" :foreground "#e4e4e4")
       (cond ((eq moe-theme-mode-line-color 'blue)
              (set-face-attribute 'mode-line nil :background "#5fafd7" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#afd7ff" :foreground "#005faf"))
             ((eq moe-theme-mode-line-color 'green)
              (set-face-attribute 'mode-line nil :background "#a1db00" :foreground "#005f00")
              (set-face-attribute 'powerline-active1 nil :background "#d7ff87" :foreground "#008700"))
             ((eq moe-theme-mode-line-color 'orange)
              (set-face-attribute 'mode-line nil :background "#ff8700" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#ffd787" :foreground "#d75f00"))
             ((eq moe-theme-mode-line-color 'magenta)
              (set-face-attribute 'mode-line nil :background "#ff4ea3" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#ffafff" :foreground "#ff1f8b"))
             ((eq moe-theme-mode-line-color 'yellow)
              (set-face-attribute 'mode-line nil :background "#fce94f" :foreground "#875f00")
              (set-face-attribute 'powerline-active1 nil :background "#ffff87" :foreground "#875f00"))
             ((eq moe-theme-mode-line-color 'purple)
              (set-face-attribute 'mode-line nil :background "#af5fd7" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#e6a8df" :foreground "#6c0099"))
             ((eq moe-theme-mode-line-color 'red)
              (set-face-attribute 'mode-line nil :background "#ff4b4b" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#ffafaf" :foreground "#cc0000"))
             ((eq moe-theme-mode-line-color 'cyan)
              (set-face-attribute 'mode-line nil :background "#5faf87" :foreground "#ffffff")
              (set-face-attribute 'powerline-active1 nil :background "#87d7af" :foreground "#005f5f"))
             ((eq moe-theme-mode-line-color 'w/b)
              (set-face-attribute 'mode-line nil :background "#ffffff" :foreground "#080808")
              (set-face-attribute 'powerline-active1 nil :background "#bcbcbc" :foreground "#3a3a3a")
              (set-face-attribute 'mode-line-buffer-id nil :background nil :foreground "#3a3a3a")))))

;; ;; Mode line setup
;; (setq-default
;;  mode-line-format
;;  '(
;;    (:eval
;;      (let ((evil-face (powerline-evil-face)))
;;        (if evil-mode
;;          (powerline-raw (concat " " (powerline-evil-tag) " ") evil-face))))
;;    ;; Position, including warning for 80 columns
;;    (:propertize "%4l:" face mode-line-position-face)
;;    (:eval (propertize "%3c" 'face
;;                       (if (>= (current-column) 80)
;;                           'mode-line-80col-face
;;                         'mode-line-position-face)))
;;    ;; emacsclient [default -- keep?]
;;    mode-line-client
;;    " "
;;    ;; read-only or modified status
;;    (:eval
;;     (cond (buffer-read-only
;;            (propertize " RO " 'face 'mode-line-read-only-face))
;;           ((buffer-modified-p)
;;            (propertize " ** " 'face 'mode-line-modified-face))
;;           (t " ")))
;;    " "
;;    ;; directory and buffer/file name
;;    (:propertize (:eval (shorten-directory default-directory 30))
;;                 face mode-line-folder-face)
;;    (:propertize "%b"
;;                 face mode-line-filename-face)
;;    ;; narrow [default -- keep?]
;;    ;;" %n "
;;    ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
;;    (vc-mode vc-mode)
;;    "  %["
;;    (:propertize mode-name face mode-line-mode-face)
;;    "%] "
;;    (:eval (propertize (format-mode-line minor-mode-alist)
;;                       'face 'mode-line-minor-mode-face))
;;    (:propertize mode-line-process
;;                 face mode-line-process-face)
;;    " "
;;    ;; mode-line-misc-info is better than Amit's version
;;    mode-line-misc-info
;;    "  "
;;    ;; nyan-mode uses nyan cat as an alternative to %p
;;    (:eval (when nyan-mode (list (nyan-create))))
;;    ))

;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; ;; Extra mode line faces
;; (make-face 'mode-line-read-only-face)
;; (make-face 'mode-line-modified-face)
;; (make-face 'mode-line-folder-face)
;; (make-face 'mode-line-filename-face)
;; (make-face 'mode-line-position-face)
;; (make-face 'mode-line-mode-face)
;; (make-face 'mode-line-minor-mode-face)
;; (make-face 'mode-line-process-face)
;; (make-face 'mode-line-80col-face)

;; (set-face-attribute 'mode-line nil
;;                     :foreground "gray60" :background "gray20"
;;                     :inverse-video nil
;;                     :box '(:line-width 2 :color "gray20" :style nil))
;; (set-face-attribute 'mode-line-inactive nil
;;                     :foreground "gray80" :background "gray40"
;;                     :inverse-video nil
;;                     :box '(:line-width 2 :color "gray40" :style nil))

;; (set-face-attribute 'mode-line-read-only-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "#4271ae"
;;                     :box '(:line-width 2 :color "#4271ae"))
;; (set-face-attribute 'mode-line-modified-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "#c82829"
;;                     :background "#ffffff"
;;                     :box '(:line-width 2 :color "#c82829"))
;; (set-face-attribute 'mode-line-folder-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "gray60")
;; (set-face-attribute 'mode-line-filename-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "#eab700"
;;                     :weight 'bold)
;; (set-face-attribute 'mode-line-position-face nil
;;                     :inherit 'mode-line-face
;;                     :family "Menlo" :height 100)
;; (set-face-attribute 'mode-line-mode-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "gray80")
;; (set-face-attribute 'mode-line-minor-mode-face nil
;;                     :inherit 'mode-line-mode-face
;;                     :foreground "gray40"
;;                     :height 110)
;; (set-face-attribute 'mode-line-process-face nil
;;                     :inherit 'mode-line-face
;;                     :foreground "#718c00")
;; (set-face-attribute 'mode-line-80col-face nil
;;                     :inherit 'mode-line-position-face
;;                     :foreground "black" :background "#eab700")

(provide 'eyecandy)

;;; theming.el ends here
