(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     ))
     ; (color-theme-hober)))

(add-to-list 'load-path "~/.emacs.d/base16-emacs.git/")
(require 'base16-eighties-theme)

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

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

