;;; solaire-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "solaire-mode" "solaire-mode.el" (0 0 0 0))
;;; Generated autoloads from solaire-mode.el

(autoload 'solaire-mode "solaire-mode" "\
Make source buffers grossly incandescent by remapping common faces (see
`solaire-mode-remap-alist') to their solaire-mode variants.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-solaire-mode "solaire-mode" "\
Enable `solaire-mode' in the current buffer.

Does nothing if it doesn't represent a real, file-visiting buffer (see
`solaire-mode-real-buffer-fn').

\(fn)" t nil)

(autoload 'turn-off-solaire-mode "solaire-mode" "\
Disable `solaire-mode' in the current buffer.

\(fn)" t nil)

(autoload 'solaire-mode-in-minibuffer "solaire-mode" "\
Highlight the minibuffer whenever it is active.

\(fn)" nil nil)

(autoload 'solaire-mode-reset "solaire-mode" "\
Reset all buffers with `solaire-mode' enabled.

\(fn &rest _)" t nil)

(autoload 'solaire-mode-swap-bg "solaire-mode" "\
Swap the backgrounds of the following faces:

+ `default' <-> `solaire-default-face'
+ `hl-line' <-> `solaire-hl-line-face'
+ `org-hide' <-> `solaire-org-hide-face'

This is necessary for themes in the doom-themes package.

\(fn)" nil nil)

(autoload 'solaire-mode-restore-persp-mode-buffers "solaire-mode" "\
Restore `solaire-mode' in buffers when `persp-mode' loads a session.

\(fn &rest _)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "solaire-mode" '("solaire-mode-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; solaire-mode-autoloads.el ends here
