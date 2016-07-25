;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Wait longer between garbage collection
;; (setq gc-cons-threshold 100000000)

;; Start Maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-message t)

;; Initialize all ELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa"        . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/" ) t)
(setq package-enable-at-startup nil)
(package-initialize)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Load use-package, used for loading packages
(require 'use-package)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(org-babel-load-file (expand-file-name "~/.emacs.d/settings.org"))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

;; Open org-default-notes-file
;; (when (file-exists-p org-default-notes-file)
;;   (find-file org-default-notes-file))
