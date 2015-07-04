;;; airline-theme.el -- Port of the vim-airline themes https://github.com/bling/vim-airline

;; Author: Anthony DiGirolamo <anthony.digirolamo@gmail.com>
;; Version: 1.0
;; Keywords: evil, mode-line, powerline, airline, theme
;; URL: http://github.com/AnthonyDiGirolamo/airline-themes.el

;;; Commentary:
;;
;; vim-airline themes for emacs powerline
;;

;;; Code:

;;;###autoload
(defun airline-theme-badwolf ()
  ""
  (interactive)
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
  (powerline-airline-set-modeline))

;;;###autoload
(defun powerline-airline-set-modeline ()
  "An attempt to imitate the spacemacs powerline theme."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (mode-line-face (if active 'mode-line 'mode-line-inactive))
                          ;; (current-evil-state-string (upcase (symbol-name evil-state)))

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
                                     ;; (powerline-buffer-id center-face)
                                     (powerline-raw "%b" center-face)

                                     ;; Current Function (which-function-mode)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       ;; (powerline-raw which-func-format 'l nil))
                                       (powerline-raw which-func-format center-face 'l))

                                     ;; ;; Separator >
                                     ;; (powerline-raw " " center-face)
                                     ;; (funcall separator-left mode-line face1)

                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object center-face 'l))

                                     ;; ;; Separator <
                                     ;; (powerline-raw " " face1)
                                     ;; (funcall separator-right face1 face2)
                                   ))

                          ;; Right Hand Side
                          (rhs (list (powerline-raw global-mode-string center-face 'r)

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
      (setq output (concat "<" output)))
    output))

(provide 'airline-themes)
;;; airline-themes.el ends here
