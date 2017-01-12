(setq pico8-highlights
      '(
        ("flr\\|pset\\|pget\\|sset\\|sget" . font-lock-builtin-face)
        ("Pi\\|Infinity" . font-lock-constant-face)
        ))

;; (defvar hexcolour-keywords
;;   '(("#[abcdef[:digit:]]\\{6\\}"
;;      (0 (put-text-property (match-beginning 0)
;;                            (match-end 0)
;;                            'face (list :background
;;                                        (match-string-no-properties 0)))))))

(define-derived-mode pico8-mode lua-mode "pico8"
  "major mode for editing pico8 code."
  ;; (setq font-lock-defaults '(pico8-highlights))
  (font-lock-add-keywords nil pico8-highlights)
)

(provide 'pico8)
;;; pico8.el ends here
