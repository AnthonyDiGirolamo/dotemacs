(require 's)

(defconst pico8-colors
  '((?^ . "white")
    (?a . "light")
    (?b . "dark")
    (?. . "black"))
  "Alist of color associations for tiles.")

(defconst pico8-palette
  '(("white" . "#ffffff")
    ("light" . "#64b0ff")
    ("dark"  . "#4240ff")
    ("black" . "#000000"))
  "Default color palette for the XPM image.")

(defvar pico8-xpm-header
"/* XPM */
static char *graphic[] = {
\"%s %s 16 1\",
\"0	c #000000\", /* black */
\"1	c #1D2B53\", /* black */
\"2	c #7E2553\", /* dark purple */
\"3	c #008751\", /* dark green */
\"4	c #AB5236\", /* brown */
\"5	c #5F574F\", /* dark gray */
\"6	c #C2C3C7\", /* light gray */
\"7	c #FFF1E8\", /* white */
\"8	c #FF004D\", /* red */
\"9	c #FFA300\", /* orange */
\"a	c #FFEC27\", /* yellow */
\"b	c #00E436\", /* green */
\"c	c #29ADFF\", /* blue */
\"d	c #83769C\", /* indigo */
\"e	c #FF77A8\", /* pink */
\"f	c #FFCCAA\", /* peach */
"
)

(defvar pico8-mode-font-lock-keywords
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
    (search-forward "\n__lua__")
    (beginning-of-line)
    (forward-line 1)
    (setq pico8-code-start (point))
    (search-forward "\n__gfx__")
    (forward-line -1)
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

(defun pico8-generate-xpm-body ()
  "Returns a string resembling a valid XPM body."
  (interactive)
  (save-excursion
   (goto-char (point-min))
   (search-forward "\n__gfx__")
   (forward-line 1)
   (beginning-of-line)
   (setq img-start (point))
   (search-forward "\n__")
   (forward-line -1)
   (end-of-line)
   (setq img-end (point))
   (let ((old-buffer (current-buffer)))
     (with-temp-buffer
     ;; (with-current-buffer "*pico8test*"
       (insert-buffer-substring-no-properties old-buffer img-start img-end)
       (goto-char (point-min))
       (end-of-line)
       (setq line-length (- (point) 1))
       (goto-char (point-min))
       (setq more-lines t)
       (while more-lines
         (beginning-of-line)
         (dotimes (i line-length)
           (forward-char 1)
           (insert (char-before)))

         (beginning-of-line)
         (setq line-start (point))
         (insert "\"")
         (end-of-line)
         (insert "\",")
         (end-of-line)
         (setq line-end (point))
         (insert (buffer-substring line-start line-end))
         (setq more-lines (= 0 (forward-line 1)))
         )
       (insert "}")
       (goto-char (point-min))
       (insert (format pico8-xpm-header 256 256))
       (setq img (create-image (buffer-string) 'xpm t))
       )
     )
   (goto-char (point-min))
   (search-forward "\n__gfx__")
   (forward-line -1)
   (insert-image img)
   )
  )


;; (with-current-buffer "heliopause.p8"
;;   (how-many (rx (sequence symbol-start "function" symbol-end))))

;; (with-current-buffer "heliopause.p8"
;;   (how-many (rx (char "[{("))))

(provide 'pico8)
;;; pico8.el ends here
