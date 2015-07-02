 ;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Initialize all ELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(setq package-enable-at-startup nil)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Load use-package, used for loading packages
(require 'use-package)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

(load "~/.emacs.d/settings.el")

(setq inhibit-startup-message t)

(setq ring-bell-function (lambda ()))
(setq inhibit-startup-message t)

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

