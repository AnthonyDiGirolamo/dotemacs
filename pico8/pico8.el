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
  (interactive)

  (save-mark-and-excursion
    (goto-char (point-min))
    (search-forward "__lua__")
    (beginning-of-line)
    (next-line)
    (setq pico8-code-start (point))
    (search-forward "__gfx__")
    (previous-line)
    (end-of-line)
    (setq pico8-code-end (point))
    (let ((old-buffer (current-buffer)))
      (with-temp-buffer
        (insert-buffer-substring-no-properties old-buffer pico8-code-start pico8-code-end)
        (goto-char (point-min))
        (replace-regexp "--\\[\\[\\(.*\n\\)*.*\\]\\]" "")
        (goto-char (point-min))
        (replace-regexp "--.*$" "")
        (goto-char (point-min))
        (replace-regexp "\"\\(\\\\\"\\|.\\)*?\"" "\"\"")
        (goto-char (point-min))
        (replace-regexp "'\\(\\\\'\\|.\\)*?'" "''")
        (lua-mode)
        (goto-char (point-min))
        (let*
            ((words           (how-many "\\_<[A-Za-z0-9_]+\\_>" ))
             (operators       (how-many "\\([-*+\\/%><~!]=?\\|[^<>~!]=[^=]\\)" ))
             (quotes          (how-many "[\"'][\"']"))
             (delimiters      (how-many "[({\\[]" ))
             (locals          (how-many "\\_<local\\_>" ))
             (ends            (how-many "\\_<end\\_>" ))
             (decimal-numbers (how-many "\\<[0-9]+\\.[0-9]+\\>" ))
             (others          (how-many "\\(::\\|#\\|\\.\\.\\.\\|[^\\.]\\.\\.[^\\.]\\)" ))
             )
          (message "%d" (- (+ words delimiters operators quotes others) locals ends decimal-numbers))
          )
        )
      )
    )
  )



;; (with-current-buffer "heliopause.p8"
;;   (how-many (rx (sequence symbol-start "function" symbol-end))))

;; (with-current-buffer "heliopause.p8"
;;   (how-many (rx (char "[{("))))

(provide 'pico8)
;;; pico8.el ends here
