(setq inhibit-startup-message t)
;; Start Maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Keep track of loading time
(defconst emacs-start-time (current-time))

(setq amd/settings-file     (expand-file-name "~/.emacs.d/README.el")
      amd/settings-org-file (expand-file-name "~/.emacs.d/README.org"))
(setq amd/uname (shell-command-to-string "uname -a"))
(setq amd/using-android (string-match "Android" amd/uname))
(setq amd/using-pocketchip (string-match "chip" amd/uname))
(setq amd/using-chromebook (string-match "galliumos" amd/uname))
(setq amd/using-pc (and (not amd/using-pocketchip)
                        (not amd/using-android)))

;; Don't garbage collect durring init
(let ((gc-cons-threshold (if amd/using-pc most-positive-fixnum 800000)))

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

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

(defun my-tangle-config-org (src-file output-file)
  "This function will write all source blocks from =config.org= into
=config.el= that are ...
- not marked as =tangle: no=
- doesn't have the TODO state =CANCELLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "Writing %s ..." output-file)
    (save-restriction
     (save-excursion
      (org-babel-map-src-blocks src-file
                                (let* ((info (org-babel-get-src-block-info 'light))
                                       (tfile (cdr (assq :tangle (nth 2 info))))
                                       (match))
                                  (save-excursion
                                   (catch 'exit
                                     (org-back-to-heading t)
                                     (when (looking-at org-outline-regexp)
                                       (goto-char (1- (match-end 0))))
                                     (when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
                                       (setq match (match-string 1)))))
                                  (unless (or (string= "no" tfile)
                                              (string= "CANCELED" match)
                                              (not (string= "emacs-lisp" lang)))
                                    (add-to-list 'body-list body)))))
     (with-temp-file output-file
       (insert (format ";; Don't edit this file, edit '%s' instead.\n\n" src-file))
       (insert (apply 'concat (reverse body-list))))
     (message "Wrote %s" output-file))))


(if (and amd/using-pocketchip (file-exists-p amd/settings-file))
    (load amd/settings-file)
  ;; (message (concat "ORG-BABEL-LOAD-FILE " amd/settings-org-file))
  (org-babel-load-file amd/settings-org-file)
  ;; (my-tangle-config-org amd/settings-org-file amd/settings-file)
  ;; (load amd/settings-file)
)

;; Message how long it took to load everything (minus packages)
(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading settings...done (%.3fs)" elapsed))

)  ;; end gc-cons-threshold let

;; Open org-default-notes-file
;; (when (file-exists-p org-default-notes-file)
;;   (find-file org-default-notes-file))
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
