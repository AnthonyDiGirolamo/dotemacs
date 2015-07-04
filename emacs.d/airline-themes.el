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

(require 'evil)
(require 'powerline)

(defface outer-normal  '((t (:foreground "#141413" :background "#aeee00" :weight normal))) "Outer Normal Face"  :group 'powerline)
(defface inner-normal  '((t (:foreground "#f4cf86" :background "#45413b" :weight normal))) "Inner Normal Face"  :group 'powerline)
(defface center-normal '((t (:foreground "#8cffba" :background "#242321" :weight normal))) "Center Normal Face" :group 'powerline)
(defface outer-insert  '((t (:foreground "#141413" :background "#0a9dff" :weight normal))) "Outer Insert Face"  :group 'powerline)
(defface inner-insert  '((t (:foreground "#f4cf86" :background "#005faf" :weight normal))) "Inner Insert Face"  :group 'powerline)
(defface center-insert '((t (:foreground "#0a9dff" :background "#242321" :weight normal))) "Center Insert Face" :group 'powerline)
(defface outer-visual  '((t (:foreground "#141413" :background "#ffa724" :weight normal))) "Outer Visual Face"  :group 'powerline)
(defface inner-visual  '((t (:foreground "#000000" :background "#fade3e" :weight normal))) "Inner Visual Face"  :group 'powerline)
(defface center-visual '((t (:foreground "#000000" :background "#b88853" :weight normal))) "Center Visual Face" :group 'powerline)

;;;###autoload
(defun airline-theme-badwolf ()
  ""
  (interactive)
  ;; Badwolf
  ;; let s:N1 = [ "#141413" , "#aeee00" , 232 , 154 ] " blackestgravel & lime
  ;; let s:N2 = [ "#f4cf86" , "#45413b" , 222 , 238 ] " dirtyblonde    & deepgravel
  ;; let s:N3 = [ "#8cffba" , "#242321" , 121 , 235 ] " saltwatertaffy & darkgravel
  ;; let s:N4 = [ "#666462" , 241 ]                   " mediumgravel

  ;; let s:I1 = [ "#141413" , "#0a9dff" , 232 , 39  ] " blackestgravel & tardis
  ;; let s:I2 = [ "#f4cf86" , "#005fff" , 222 , 27  ] " dirtyblonde    & facebook
  ;; let s:I3 = [ "#0a9dff" , "#242321" , 39  , 235 ] " tardis         & darkgravel

  ;; let s:V1 = [ "#141413" , "#ffa724" , 232 , 214 ] " blackestgravel & orange
  ;; let s:V2 = [ "#000000" , "#fade3e" , 16  , 221 ] " coal           & dalespale
  ;; let s:V3 = [ "#000000" , "#b88853" , 16  , 137 ] " coal           & toffee
  ;; let s:V4 = [ "#c7915b" , 173 ]                   " coffee

  (set-face-attribute 'outer-normal        nil :foreground "#141413" :background "#aeee00")
  (set-face-attribute 'inner-normal        nil :foreground "#f4cf86" :background "#45413b")
  (set-face-attribute 'center-normal       nil :foreground "#8cffba" :background "#242321")

  (set-face-attribute 'outer-insert        nil :foreground "#141413" :background "#0a9dff")
  (set-face-attribute 'inner-insert        nil :foreground "#f4cf86" :background "#005faf")
  (set-face-attribute 'center-insert       nil :foreground "#0a9dff" :background "#242321")

  (set-face-attribute 'outer-visual        nil :foreground "#141413" :background "#ffa724")
  (set-face-attribute 'inner-visual        nil :foreground "#000000" :background "#fade3e")
  (set-face-attribute 'center-visual       nil :foreground "#000000" :background "#b88853")

  (set-face-attribute 'mode-line           nil :foreground "#8cffba" :background "#242321")

  (set-face-attribute 'powerline-inactive1 nil :foreground "#45413b" :background "#141413")
  (set-face-attribute 'powerline-inactive2 nil :foreground "#45413b" :background "#242321")

  (powerline-airline-set-modeline)
  (powerline-reset))

;;;###autoload
(defun airline-theme-light ()
  ""
  (interactive)
  ;; Light
  ;; let s:N1 = [ "#ffffff" , "#005fff" , 255 , 27  ]
  ;; let s:N2 = [ "#000087" , "#00dfff" , 18  , 45  ]
  ;; let s:N3 = [ "#005fff" , "#afffff" , 27  , 159 ]

  ;; let s:I1 = [ "#ffffff" , "#00875f" , 255 , 29  ]
  ;; let s:I2 = [ "#005f00" , "#00df87" , 22  , 42  ]
  ;; let s:I3 = [ "#005f5f" , "#afff87" , 23  , 156 ]

  ;; let s:V1 = [ "#ffffff" , "#ff5f00" , 255 , 202 ]
  ;; let s:V2 = [ "#5f0000" , "#ffaf00" , 52  , 214 ]
  ;; let s:V3 = [ "#df5f00" , "#ffff87" , 166 , 228 ]

  ;; let s:IA1 = [ "#666666" , "#b2b2b2" , 242 , 249 , "" ]
  ;; let s:IA2 = [ "#8a8a8a" , "#d0d0d0" , 245 , 252 , "" ]
  ;; let s:IA3 = [ "#a8a8a8" , "#ffffff" , 248 , 255 , "" ]

  (set-face-attribute 'outer-normal        nil :foreground "#ffffff" :background "#005fff")
  (set-face-attribute 'inner-normal        nil :foreground "#000087" :background "#00dfff")
  (set-face-attribute 'center-normal       nil :foreground "#005f5f" :background "#afffff")

  (set-face-attribute 'outer-insert        nil :foreground "#ffffff" :background "#00875f")
  (set-face-attribute 'inner-insert        nil :foreground "#005f00" :background "#00df87")
  (set-face-attribute 'center-insert       nil :foreground "#005f5f" :background "#afff87")

  (set-face-attribute 'outer-visual        nil :foreground "#ffffff" :background "#ff5f00")
  (set-face-attribute 'inner-visual        nil :foreground "#5f0000" :background "#ffaf00")
  (set-face-attribute 'center-visual       nil :foreground "#df5f00" :background "#ffff87")

  (set-face-attribute 'mode-line           nil :foreground "#005f5f" :background "#afffff")

  (set-face-attribute 'powerline-inactive1 nil :foreground "#8a8a8a" :background "#d0d0d0")
  (set-face-attribute 'powerline-inactive2 nil :foreground "#a8a8a8" :background "#ffffff")

  (powerline-airline-set-modeline)
  (powerline-reset))

;;;###autoload
(defun airline-theme-dark ()
  ""
  (interactive)
  ;; Dark
  ;; let s:N1  = [ "#00005f" , "#dfff00" , 17  , 190 ]
  ;; let s:N2  = [ "#ffffff" , "#444444" , 255 , 238 ]
  ;; let s:N3  = [ "#9cffd3" , "#202020" , 85  , 234 ]

  ;; let s:I1  = [ "#00005f" , "#00dfff" , 17  , 45  ]
  ;; let s:I2  = [ "#ffffff" , "#005fff" , 255 , 27  ]
  ;; let s:I3  = [ "#ffffff" , "#000080" , 15  , 17  ]

  ;; let s:V1  = [ "#000000" , "#ffaf00" , 232 , 214 ]
  ;; let s:V2  = [ "#000000" , "#ff5f00" , 232 , 202 ]
  ;; let s:V3  = [ "#ffffff" , "#5f0000" , 15  , 52  ]

  ;; let s:IA1 = [ "#4e4e4e" , "#1c1c1c" , 239 , 234 , "" ]
  ;; let s:IA2 = [ "#4e4e4e" , "#262626" , 239 , 235 , "" ]
  ;; let s:IA3 = [ "#4e4e4e" , "#303030" , 239 , 236 , "" ]

  (set-face-attribute 'outer-normal        nil :foreground "#00005f" :background "#dfff00")
  (set-face-attribute 'inner-normal        nil :foreground "#ffffff" :background "#444444")
  (set-face-attribute 'center-normal       nil :foreground "#9cffd3" :background "#202020")

  (set-face-attribute 'outer-insert        nil :foreground "#00005f" :background "#00dfff")
  (set-face-attribute 'inner-insert        nil :foreground "#ffffff" :background "#005fff")
  (set-face-attribute 'center-insert       nil :foreground "#ffffff" :background "#000080")

  (set-face-attribute 'outer-visual        nil :foreground "#000000" :background "#ffaf00")
  (set-face-attribute 'inner-visual        nil :foreground "#000000" :background "#ff5f00")
  (set-face-attribute 'center-visual       nil :foreground "#ffffff" :background "#5f0000")

  (set-face-attribute 'mode-line           nil :foreground "#9cffd3" :background "#202020")

  (set-face-attribute 'powerline-inactive1 nil :foreground "#4e4e4e" :background "#1c1c1c")
  (set-face-attribute 'powerline-inactive2 nil :foreground "#4e4e4e" :background "#262626")

  (powerline-airline-set-modeline)
  (powerline-reset))

;;;###autoload
(defun airline-theme-powerlineish ()
  ""
  (interactive)
  ;; powerlineish
  ;; let s:N1 = [ "#005f00" , "#afd700" , 22  , 148 ] " darkestgreen   & brightgreen
  ;; let s:N2 = [ "#9e9e9e" , "#303030" , 247 , 236 ] " gray8          & gray2
  ;; let s:N3 = [ "#ffffff" , "#121212" , 231 , 233 ] " white          & gray4

  ;; let s:I1 = [ "#005f5f" , "#ffffff" , 23  , 231 ] " darkestcyan    & white
  ;; let s:I2 = [ "#5fafd7" , "#0087af" , 74  , 31  ] " darkcyan       & darkblue
  ;; let s:I3 = [ "#87d7ff" , "#005f87" , 117 , 24  ] " mediumcyan     & darkestblue

  ;; let s:V1 = [ "#080808" , "#ffaf00" , 232 , 214 ] " gray3          & brightestorange
  ;; let s:N2 = [ "#9e9e9e" , "#303030" , 247 , 236 ] " gray8          & gray2
  ;; let s:N3 = [ "#ffffff" , "#121212" , 231 , 233 ] " white          & gray4

  ;; let s:RE = [ "#ffffff" , "#d70000" , 231 , 160 ] " white          & brightred
  ;; let s:N2 = [ "#9e9e9e" , "#303030" , 247 , 236 ] " gray8          & gray2
  ;; let s:N3 = [ "#ffffff" , "#121212" , 231 , 233 ] " white          & gray4

  (set-face-attribute 'outer-normal        nil :foreground "#005f00" :background "#afd700")
  (set-face-attribute 'inner-normal        nil :foreground "#9e9e9e" :background "#303030")
  (set-face-attribute 'center-normal       nil :foreground "#ffffff" :background "#121212")

  (set-face-attribute 'outer-insert        nil :foreground "#005f5f" :background "#ffffff")
  (set-face-attribute 'inner-insert        nil :foreground "#5fafd7" :background "#0087af")
  (set-face-attribute 'center-insert       nil :foreground "#87d7ff" :background "#005f87")

  (set-face-attribute 'outer-visual        nil :foreground "#080808" :background "#ffaf00")
  (set-face-attribute 'inner-visual        nil :foreground "#9e9e9e" :background "#303030")
  (set-face-attribute 'center-visual       nil :foreground "#ffffff" :background "#121212")

  (set-face-attribute 'mode-line           nil :foreground "#ffffff" :background "#121212")

  (set-face-attribute 'powerline-inactive1 nil :foreground "#45413b" :background "#141413")
  (set-face-attribute 'powerline-inactive2 nil :foreground "#45413b" :background "#242321")

  (powerline-airline-set-modeline)
  (powerline-reset))

;;;###autoload
(defun powerline-airline-set-modeline ()
  "Set the airline mode-line-format"
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
                          (visual-block (and (evil-visual-state-p)
                                             (eq evil-visual-selection 'block)))
                          (visual-line (and (evil-visual-state-p)
                                            (eq evil-visual-selection 'line)))
                          (current-evil-state-string (upcase (concat (symbol-name evil-state)
                                                                     (cond (visual-block " BLOCK")
                                                                           (visual-line " LINE")))))

                          ;; Left Hand Side
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

                          (lhs (list (when evil-mode
                                       (powerline-raw (concat " " current-evil-state-string " ") outer-face))
                                     (when evil-mode
                                       (funcall separator-left outer-face inner-face))

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
