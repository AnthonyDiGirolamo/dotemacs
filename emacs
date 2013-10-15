; Colors
; launch emacs with TERM=xterm-16color
(add-to-list 'custom-theme-load-path "~/.emacs.d/base16-emacs.git")
(load-theme 'base16-eighties t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized.git")
; (load-theme 'solarized-dark t)

; load evil (emacs vi layer)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

; mu4e email suite
(require 'mu4e)
(setq
  mu4e-maildir       "~/Mail"         ;; top-level Maildir
  mu4e-sent-folder   "/Sent Items"    ;; folder for sent messages
  mu4e-drafts-folder "/Drafts"        ;; unfinished messages
  mu4e-trash-folder  "/Deleted Items" ;; trashed messages
  mu4e-refile-folder "/Archive")      ;; saved messages
(setq
  mu4e-get-mail-command "offlineimap" ;; or fetchmail, or ...
  mu4e-update-interval 300)           ;; update every 5 minutes
(setq mail-user-agent 'mu4e-user-agent)

