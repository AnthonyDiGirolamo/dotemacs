;; Keep track of loading time
(defconst emacs-start-time (current-time))

;; Wait longer between garbage collection
(setq gc-cons-threshold 100000000)

;; Start Maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-message t)

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file 'noerror))

;; Initialize all ELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/" ) t)
(setq package-enable-at-startup nil)
(package-initialize)

;; (benchmark-init/activate)

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Loaded packages in %.3fs" elapsed))

;; Load use-package, used for loading packages
(require 'use-package)

;; (defun my-tangle-config-org ()
;;   "This function will write all source blocks from =config.org= into
;; =config.el= that are ...
;; - not marked as =tangle: no=
;; - doesn't have the TODO state =CANCELLED=
;; - have a source-code of =emacs-lisp="
;;   (require 'org)
;;   (let* ((body-list ())
;;          (output-file "settings.el")
;;          (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
;;                                                                 (list (cons :tangle output-file)))))
;;     (message "Writing %s ..." output-file)
;;     (save-restriction
;;       (save-excursion
;;         (org-babel-map-src-blocks "settings.org"
;;                                   (let* ((info (org-babel-get-src-block-info 'light))
;;                                          (tfile (cdr (assq :tangle (nth 2 info))))
;;                                          (match))
;;                                     (save-excursion
;;                                       (catch 'exit
;;                                         (org-back-to-heading t)
;;                                         (when (looking-at org-outline-regexp)
;;                                           (goto-char (1- (match-end 0))))
;;                                         (when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
;;                                           (setq match (match-string 1)))))
;;                                     (unless (or (string= "no" tfile)
;;                                                 (string= "CANCELED" match)
;;                                                 (not (string= "emacs-lisp" lang)))
;;                                       (add-to-list 'body-list body)))))
;;       (with-temp-file output-file
;;         (insert ";; Don't edit this file, edit settings.org' instead ...\n\n")
;;         (insert (apply 'concat (reverse body-list))))
;;       (message "Wrote %s ..." output-file))))

;; (my-tangle-config-org)
(setq amd/settings-file (expand-file-name "~/.emacs.d/settings.el")
      amd/settings-org-file (expand-file-name "~/.emacs.d/settings.org"))
(setq amd/uname (shell-command-to-string "uname -a"))
(setq amd/using-android (string-match "Android" amd/uname))
(setq amd/using-pocketchip (string-match "chip" amd/uname))

(if (and amd/using-pocketchip (file-exists-p amd/settings-file))
    (load amd/settings-file)
  (message (concat "ORG-BABEL-LOAD-FILE " amd/settings-org-file))
  (org-babel-load-file amd/settings-org-file))

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

;; Open org-default-notes-file
;; (when (file-exists-p org-default-notes-file)
;;   (find-file org-default-notes-file))