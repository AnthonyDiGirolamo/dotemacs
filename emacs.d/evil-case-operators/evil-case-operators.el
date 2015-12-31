;;; evil-case-operators.el --- Evil operators for changing case

;; Author: Anthony DiGirolamo <anthony.digirolamo@gmail.com>
;; URL: http://github.com/AnthonyDiGirolamo/evil-case-operators
;; Version: 0.1
;; Keywords: evil, plugin
;; Package-Requires: ((evil "1.0.7"))

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides some extra operators for Emacs Evil, to evaluate codes,
;; search via google, translate text, folding region, etc.
;;
;; Installation:
;;
;; put evil-case-operators.el somewhere in your load-path and add these
;; lines to your .emacs:
;; (require 'evil-case-operators)
;; (global-evil-case-operators-mode 1)

;;; Code:

(require 'evil)

(defgroup evil-case-operators nil
  "Extra operator for evil"
  :prefix "evil-case-operators"
  :group 'evil)

;; (defcustom evil-case-operators-fold-key (kbd "gs")
;;   "Default binding for evil-operator-fold."
;;   :type 'key-sequence
;;   :group 'evil-case-operators)

(defcustom evil-case-operators-toggle-variable-case-key (kbd "gl")
  "Default binding for evil-operator-fold."
  :type 'key-sequence
  :group 'evil-case-operators)

;; ;;;###autoload
;; (autoload 'evil-operator-fold "evil-case-operators"
;;   "Evil operator for folding region." t)

;;;###autoload
(autoload 'evil-case-operators-toggle-variable-case "evil-case-operators"
  "Evil operator for toggling variable name case styles." t)

;; (evil-define-operator evil-operator-fold (beg end type)
;;   "Evil operator for folding region."
;;   :move-point nil
;;   (interactive "<R>")
;;   (require 'fold-this)
;;   (if (eq type 'block)
;;       (evil-apply-on-block #'fold-this beg end nil)
;;     (fold-this beg end)))

(evil-define-operator evil-case-operators-toggle-variable-case (beg end)
  "Evil operator for toggling variable name case styles."
  :move-point nil
  (interactive "<r>")
  (require 's)
  (let* ((original-name  (buffer-substring-no-properties beg end))
         (possible-names (list (s-dashed-words original-name)
                               (s-snake-case original-name)
                               (s-lower-camel-case original-name)
                               (s-upper-camel-case original-name)))
         (original-index (cl-position original-name possible-names :test 'equal))
         (new-index      (mod (+ 1 (or original-index 0)) (length possible-names))))
    (save-excursion
      (delete-region beg end)
      (insert (nth new-index possible-names)))
    )
  )

;;;###autoload
(define-minor-mode evil-case-operators-mode
  "Buffer local minor mode to enable extra operators for Evil."
  :lighter ""
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

;;;###autoload
(defun evil-case-operators-mode-install () (evil-case-operators-mode 1))

;;;###autoload
(define-globalized-minor-mode global-evil-case-operators-mode
  evil-case-operators-mode evil-case-operators-mode-install
  "Global minor mode of extra operator for Evil.")

;; (evil-define-key 'motion evil-case-operators-mode-map
;;   evil-case-operators-fold-key
;;   'evil-operator-fold)
;; (evil-define-key 'normal evil-case-operators-mode-map
;;   evil-case-operators-fold-key
;;   'evil-operator-fold)

(evil-define-key 'motion evil-case-operators-mode-map
  evil-case-operators-toggle-variable-case-key
  'evil-case-operators-toggle-variable-case)
(evil-define-key 'normal evil-case-operators-mode-map
  evil-case-operators-toggle-variable-case-key
  'evil-case-operators-toggle-variable-case)

(provide 'evil-case-operators)
;;; evil-case-operators.el ends here
