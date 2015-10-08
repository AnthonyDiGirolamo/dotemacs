;;; ob-ocaml.el --- org-babel functions for ocaml evaluation

;; Copyright (C) 2009-2015 Free Software Foundation, Inc.

;; Author: Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating ocaml source code.  This one will
;; be sort of tricky because ocaml programs must be compiled before
;; they can be run, but ocaml code can also be run through an
;; interactive interpreter.
;;
;; For now lets only allow evaluation using the ocaml interpreter.

;;; Requirements:

;; - tuareg-mode :: http://www-rocq.inria.fr/~acohen/tuareg/

;;; Code:
(require 'ob)
(require 'comint)
(eval-when-compile (require 'cl))

(declare-function tuareg-run-caml "ext:tuareg" ())
(declare-function tuareg-run-ocaml "ext:tuareg" ())
(declare-function tuareg-interactive-send-input "ext:tuareg" ())

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("ocaml" . "ml"))

(defvar org-babel-default-header-args:ocaml '())

(defvar org-babel-ocaml-eoe-indicator "\"org-babel-ocaml-eoe\";;")
(defvar org-babel-ocaml-eoe-output "org-babel-ocaml-eoe")

(defcustom org-babel-ocaml-command "ocaml"
  "Name of the command for executing Ocaml code."
  :version "24.4"
  :package-version '(Org . "8.0")
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:ocaml (body params)
  "Execute a block of Ocaml code with Babel."
  (let* ((vars (mapcar #'cdr (org-babel-get-header params :var)))
         (full-body (org-babel-expand-body:generic
		     body params
		     (org-babel-variable-assignments:ocaml params)))
         (session (org-babel-prep-session:ocaml
		   (cdr (assoc :session params)) params))
         (raw (org-babel-comint-with-output
		  (session org-babel-ocaml-eoe-output t full-body)
		(insert
		 (concat
		  (org-babel-chomp full-body) ";;\n"
		  org-babel-ocaml-eoe-indicator))
		(tuareg-interactive-send-input)))
	 (clean
	  (car (let ((re (regexp-quote org-babel-ocaml-eoe-output)) out)
		 (delq nil (mapcar (lambda (line)
				     (if out
					 (progn (setq out nil) line)
				       (when (string-match re line)
					 (progn (setq out t) nil))))
				   (mapcar #'org-babel-trim (reverse raw)))))))
	 (raw (org-babel-trim clean))
	 (result-params (cdr (assoc :result-params params)))
	 (parsed 
	  (string-match 
	   "\\(\\(.*\n\\)*\\)[^:\n]+ : \\([^=\n]+\\) =\\(\n\\| \\)\\(.+\\)$" 
	   raw))
	 (output (match-string 1 raw))
	 (type (match-string 3 raw))
	 (value (match-string 5 raw)))
    (org-babel-reassemble-table
     (org-babel-result-cond result-params
       (cond
	((member "verbatim" result-params) raw)
	((member "output" result-params) output)
	(t raw))
       (if (and value type)
	   (org-babel-ocaml-parse-output value type)
	 raw))
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

(defvar tuareg-interactive-buffer-name)
(defun org-babel-prep-session:ocaml (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (require 'tuareg)
  (let ((tuareg-interactive-buffer-name (if (and (not (string= session "none"))
                                                 (not (string= session "default"))
                                                 (stringp session))
                                            session
                                          tuareg-interactive-buffer-name)))
    (save-window-excursion (if (fboundp 'tuareg-run-process-if-needed)
	 (tuareg-run-process-if-needed org-babel-ocaml-command)
       (tuareg-run-caml)))
    (get-buffer tuareg-interactive-buffer-name)))

(defun org-babel-variable-assignments:ocaml (params)
  "Return list of ocaml statements assigning the block's variables."
  (mapcar
   (lambda (pair) (format "let %s = %s;;" (car pair)
			  (org-babel-ocaml-elisp-to-ocaml (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

(defun org-babel-ocaml-elisp-to-ocaml (val)
  "Return a string of ocaml code which evaluates to VAL."
  (if (listp val)
      (concat "[|" (mapconcat #'org-babel-ocaml-elisp-to-ocaml val "; ") "|]")
    (format "%S" val)))

(defun org-babel-ocaml-parse-output (value type)
  "Parse VALUE of type TYPE.
VALUE and TYPE are string output from an ocaml process."
  (cond
   ((string= "string" type)
    (org-babel-read value))
   ((or (string= "int" type)
	(string= "float" type))
    (string-to-number value))
   ((string-match "list" type)
    (org-babel-ocaml-read-list value))
   ((string-match "array" type)
    (org-babel-ocaml-read-array value))
   (t (message "don't recognize type %s" type) value)))

(defun org-babel-ocaml-read-list (results)
  "Convert RESULTS into an elisp table or string.
If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  ;; XXX: This probably does not behave as expected when a semicolon
  ;; is in a string in a list.  The same comment applies to
  ;; `org-babel-ocaml-read-array' below (with even more failure
  ;; modes).
  (org-babel-script-escape (replace-regexp-in-string ";" "," results)))

(defun org-babel-ocaml-read-array (results)
  "Convert RESULTS into an elisp table or string.
If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  (org-babel-script-escape
   (replace-regexp-in-string
    "\\[|" "[" (replace-regexp-in-string
		"|\\]" "]" (replace-regexp-in-string
			    "; " "," results)))))

(provide 'ob-ocaml)



;;; ob-ocaml.el ends here