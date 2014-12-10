;;; helm-projectile-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (helm-projectile helm-projectile-ag helm-projectile-ack
;;;;;;  helm-projectile-grep helm-projectile-off helm-projectile-on
;;;;;;  helm-projectile-find-other-file) "helm-projectile" "../../../../../.emacs.d/elpa/helm-projectile-20141208.716/helm-projectile.el"
;;;;;;  "adc082f68de39f14ca22488e7fa19d26")
;;; Generated autoloads from ../../../../../.emacs.d/elpa/helm-projectile-20141208.716/helm-projectile.el

(autoload 'helm-projectile-find-other-file "helm-projectile" "\
Switch between files with the same name but different extensions using Helm.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'.

\(fn &optional FLEX-MATCHING)" t nil)

(autoload 'helm-projectile-on "helm-projectile" "\
Turn on helm-projectile key bindings.

\(fn)" t nil)

(autoload 'helm-projectile-off "helm-projectile" "\
Turn off helm-projectile key bindings.

\(fn)" t nil)

(autoload 'helm-projectile-grep "helm-projectile" "\
Helm version of projectile-grep.

\(fn)" t nil)

(autoload 'helm-projectile-ack "helm-projectile" "\
Helm version of projectile-ack.

\(fn)" t nil)

(autoload 'helm-projectile-ag "helm-projectile" "\
Helm version of projectile-ag.

\(fn)" t nil)

(autoload 'helm-projectile "helm-projectile" "\
Use projectile with Helm instead of ido.

With a prefix ARG invalidates the cache first.
If invoked outside of a project, displays a list of known projects to jump.

\(fn &optional ARG)" t nil)

(eval-after-load 'projectile '(progn (define-key projectile-command-map (kbd "h") 'helm-projectile)))

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/elpa/helm-projectile-20141208.716/helm-projectile-pkg.el"
;;;;;;  "../../../../../.emacs.d/elpa/helm-projectile-20141208.716/helm-projectile.el")
;;;;;;  (21639 35436 528745 0))

;;;***

(provide 'helm-projectile-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; helm-projectile-autoloads.el ends here
