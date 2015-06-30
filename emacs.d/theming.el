;;; theming.el -- Theming configuration for my Emacs

;;; Commentary:
;;; Author : ashutoshrishi

;;; Code:

(defface powerline-custom1 '((t (:background "#EEAD0E" :foreground "black" :weight bold)))
  "Custom face for bright sections"
  :group 'powerline)

(defface powerline-custom2 '((t (:foreground "#EEAD0E" :weight bold)))
  "Custom face for text"
  :group 'powerline)

(require 'anzu)
(defun my/anzu-update-func (here total)
  "Customizing how anzu displays HERE & TOTAL on the mode line."
  (propertize (format " <%d/%d>" here total)
              'face 'powerline-custom1))
(setq anzu-mode-line-update-function 'my/anzu-update-func)

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
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (concat " " (powerline-evil-tag) " ") evil-face)))
                                     (powerline-raw "%*" face3 'l)
                                     (powerline-raw " " face3)
                                     (funcall separator-left face3 mode-line)

                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size nil 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info face4 'l))
                                     (powerline-buffer-id face4 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)

                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-raw " " face1)
                                     (funcall separator-right face1 mode-line)

                                     (powerline-minor-modes mode-line 'l)
                                     (powerline-narrow mode-line 'l)
                                     (powerline-raw " " mode-line)
                                     (funcall separator-left mode-line face1)

                                     (powerline-vc face1 'r)
                                     (powerline-raw " " face1)
                                     (funcall separator-right face1 face2)

                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face1 'l))
				     (powerline-raw "%4l" face1 'l)
				     (powerline-raw ":" face1 'l)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%6p" nil 'r))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

;; (defun powerline-default-theme ()
;;   "Setup the default mode-line."
;;   (interactive)
;;   (setq-default mode-line-format
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (mode-line (if active 'mode-line 'mode-line-inactive))
;;                           (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                           (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                           (separator-left (intern (format "powerline-%s-%s"
;; 							  (powerline-current-separator)
;;                                                           (car powerline-default-separator-dir))))
;;                           (separator-right (intern (format "powerline-%s-%s"
;;                                                            (powerline-current-separator)
;;                                                            (cdr powerline-default-separator-dir))))
;;                           (lhs (list (powerline-raw "%*" nil 'l)
;;                                      (when powerline-display-buffer-size
;;                                        (powerline-buffer-size nil 'l))
;;                                      (when powerline-display-mule-info
;;                                        (powerline-raw mode-line-mule-info nil 'l))
;;                                      (powerline-buffer-id nil 'l)
;;                                      (when (and (boundp 'which-func-mode) which-func-mode)
;;                                        (powerline-raw which-func-format nil 'l))
;;                                      (powerline-raw " ")
;;                                      (funcall separator-left mode-line face1)
;;                                      (when (boundp 'erc-modified-channels-object)
;;                                        (powerline-raw erc-modified-channels-object face1 'l))
;;                                      (powerline-major-mode face1 'l)
;;                                      (powerline-process face1)
;;                                      (powerline-minor-modes face1 'l)
;;                                      (powerline-narrow face1 'l)
;;                                      (powerline-raw " " face1)
;;                                      (funcall separator-left face1 face2)
;;                                      (powerline-vc face2 'r)
;;                                      (when (bound-and-true-p nyan-mode)
;;                                        (powerline-raw (list (nyan-create)) face2 'l))))
;;                           (rhs (list (powerline-raw global-mode-string face2 'r)
;;                                      (funcall separator-right face2 face1)
;; 				     (unless window-system
;; 				       (powerline-raw (char-to-string #xe0a1) face1 'l))
;; 				     (powerline-raw "%4l" face1 'l)
;; 				     (powerline-raw ":" face1 'l)
;; 				     (powerline-raw "%3c" face1 'r)
;; 				     (funcall separator-right face1 mode-line)
;; 				     (powerline-raw " ")
;; 				     (powerline-raw "%6p" nil 'r)
;;                                      (when powerline-display-hud
;;                                        (powerline-hud face2 face1)))))
;; 		     (concat (powerline-render lhs)
;; 			     (powerline-fill face2 (powerline-width rhs))
;; 			     (powerline-render rhs)))))))

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

(provide 'theming)

;;; theming.el ends here
