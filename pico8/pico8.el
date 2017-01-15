(require 's)

;; emacs effortless major-mode development

(setq pico8-mode-font-lock-keywords
      `(
        (,(regexp-opt '("sget" "sset" "fget" "fset" "clip" "print" "cursor" "color" "cls" "camera" "circ" "circfill" "line" "rect" "rectfill" "pal" "palt" "spr" "sspr" "btn" "btnp" "pset" "pget" "sfx" "music" "mset" "mget" "peek" "poke" "memcpy" "reload" "cstore" "memset" "min" "max" "mid" "flr" "cos" "sin" "atan2" "sqrt" "abs" "rnd" "srand" "band" "bor" "bxor" "bnot" "shr" "shl" "menuitem" "cartdata" "dget" "dset") 'symbols)
         . font-lock-builtin-face)
        )
      )

;; (defvar hexcolour-keywords
;;   '(("#[abcdef[:digit:]]\\{6\\}"
;;      (0 (put-text-property (match-beginning 0)
;;                            (match-end 0)
;;                            'face (list :background
;;                                        (match-string-no-properties 0)))))))

(define-derived-mode pico8-mode lua-mode "pico8"
  "major mode for editing pico8 code."
  (font-lock-add-keywords nil pico8-mode-font-lock-keywords)
)

;; (defun run-pico8 ()
;;   "run a pico-8 cartridge then revert buffer"
;;   (interactive)
;;   (let ((current-file-path (file-name-base (buffer-file-name (current-buffer))))
;;         (pico8-command     (cond ((eq system-type 'cygwin)
;;                                   "/home/anthony/pico-8_win32/pico8.exe -windowed 1 -home 'C:\cygwin64\home\anthony\heliopause-pico-8' -run "
;;                                   ;; "/home/anthony/pico-8_win32/pico8.exe -windowed 1 -home C:/cygwin64/home/anthony/heliopause-pico-8 "
;;                                   )
;;                                  ((eq system-type 'windows-nt)
;;                                   "c:/Users/anthony/pico-8_win32/pico8.exe -windowed 1 -home C:\\Users\\anthony\\heliopause-pico-8 -run "
;;                                   )
;;                                  (amd/using-pocketchip
;;                                   "/usr/lib/pico8/pico8 -run ")
;;                                  (t
;;                                   "/home/anthony/apps/pico-8/pico8 -run "))))
;;     (save-buffer)
;;     (shell-command (concat pico8-command current-file-path))
;;     ;; (shell-command pico8-command)
;;     (revert-buffer nil t)))

(defun pico8-token-count ()
  "Return token counts"
  ;; (buffer)
  ;; (interactive "b")
  (interactive)
  ;; (message "%s" (current-buffer))
  ;; (with-current-buffer (current-buffer)
  (message "%d" (how-many (regexp-opt '("local") 'symbols)))
  ;; )
  ;; (save-excursion
  ;;   (let* (
  ;;          (local-count (how-many (rx-(rx (sequence symbol-start "local" symbol-end))))
  ;;          )
  ;;     (message "%d" local-count)
  ;;     ))
  ;; (rx-to-string (rx (sequence word-start "local" word-end)))
  )



;; (with-current-buffer "heliopause.p8"
;;   (how-many (rx (sequence symbol-start "function" symbol-end))))

;; (with-current-buffer "heliopause.p8"
;;   (how-many (rx (char "[{("))))

(provide 'pico8)
;;; pico8.el ends here
