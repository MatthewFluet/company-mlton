;;; company-mlton.el --- company-mode backend for MLton/Standard ML  -*- lexical-binding: t -*-

;; Copyright (C) 2017  Matthew Fluet

;; Author: Matthew Fluet <Matthew.Fluet@gmail.com>
;; URL: https://github.com/MatthewFluet/company-mlton
;; Version: 1.0
;; Keywords: company-mode mlton standard-ml
;; Package-Requires:

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary


;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup company-mlton nil
  "Completion backend for MLton/SML."
  :group 'company)

(defvar company-mlton-modes '(sml-mode)
  "Major modes in which company-mlton may complete.")

(defconst company-mlton-keyword--sml-keywords-core
  '("abstype" "and" "andalso" "as" "case" "datatype" "do" "else"
    "end" "exception" "fn" "fun" "handle" "if" "in" "infix"
    "infixr" "let" "local" "nonfix" "of" "op" "open" "orelse"
    "raise" "rec" "then" "type" "val" "with" "withtype" "while"))
(defconst company-mlton-keyword--sml-keywords-modules
  '("eqtype" "functor" "include" "sharing" "sig"
    "signature" "struct" "structure" "where"))
(defconst company-mlton-keyword--sml-keywords
  (sort (append company-mlton-keyword--sml-keywords-core
                company-mlton-keyword--sml-keywords-modules)
        'string<))

(defvar-local company-mlton-basis--ids 'nil)

(defconst company-mlton--sml-alphanum-re
  "\\(?:[A-Za-z0-9'_]\\)")
(defconst company-mlton--sml-alphanum-id-re
  (concat "\\(?:"
          "[A-Za-z]" company-mlton--sml-alphanum-re "*"
          "\\)"))
(defconst company-mlton--sml-sym-re
  "\\(?:[!%&$#+-/:<=>?@\\~`^|*]\\)")
(defconst company-mlton--sml-sym-id-re
  (concat "\\(?:"
          company-mlton--sml-sym-re "+"
          "\\)"))
(defconst company-mlton--sml-long-id-re
  (concat "\\(?:"
          "\\(?:" company-mlton--sml-alphanum-id-re "[.]" "\\)*"
          "\\(?:" company-mlton--sml-alphanum-id-re "\\|" company-mlton--sml-sym-id-re "\\)"
          "\\)"))
(defconst company-mlton--sml-tyvar-id-re
  (concat "\\(?:"
          "'" company-mlton--sml-alphanum-re "*"
          "\\)"))
(defconst company-mlton--sml-tyvars-re
  (concat "\\(?:"
          " *" company-mlton--sml-tyvar-id-re
          "\\|"
          " *" "(" " *" company-mlton--sml-tyvar-id-re "\\(?:" " *" "," " *" company-mlton--sml-tyvar-id-re "\\)*" " *" ")"
          "\\)?"))

(defun company-mlton--prefix ()
  (buffer-substring (point)
                    (save-excursion
                      (skip-chars-backward "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789!%&$#+-/:<=>?@\\~`^|*.")
                      (point))))

(defconst company-mlton-basis--entry-annot-id-re
  (concat "^"
          "\\("
          "type" company-mlton--sml-tyvars-re
          "\\|"
          "datatype" company-mlton--sml-tyvars-re
          "\\|"
          "con"
          "\\|"
          "exn"
          "\\|"
          "val"
          "\\|"
          "signature"
          "\\|"
          "structure"
          "\\|"
          "functor"
          "\\)"
          " +"
          "\\(" company-mlton--sml-long-id-re "\\)"
          " *[:=][ \n]"))
(defconst company-mlton-basis--entry-location-re
  "[ ]+(\\* @ \\(.*\\) \\([0-9]+\\).\\([0-9]+\\)-\\([0-9]+\\).\\([0-9]+\\) \\*)$")

(defun company-mlton-basis--load-ids-from-file (file)
  (setq-local
   company-mlton-basis--ids
   (with-temp-buffer
     (insert-file-contents file)
     (goto-char (point-min))
     (let* ((ids nil))
       (while (re-search-forward company-mlton-basis--entry-annot-id-re nil t)
         (let* ((meta-start (match-beginning 0))
                (annotation (pcase (substring (match-string 1) 0 3)
                              ("dat" "typ")
                              ("fun" "fct")
                              (ann ann)))
                (id (match-string 2)))
           (re-search-forward company-mlton-basis--entry-location-re nil t)
           (let* ((meta-end (match-beginning 0))
                  (meta (replace-regexp-in-string "[ \n]+$" " " (buffer-substring meta-start meta-end)))
                  (file (match-string 1))
                  (line (string-to-number (match-string 2)))
                  (location (cons file line)))
             (setq ids
                   (cons (propertize id
                                     'annotation annotation
                                     'meta meta
                                     'location location)
                         ids)))))
       ids))))

(defun company-mlton-basis-load ()
  (interactive)
  (company-mlton-basis--load-ids-from-file (read-file-name "Specify basis file: " nil nil t nil nil)))

;;;###autoload
(defun company-mlton-keyword (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-mlton-keyword))
    (prefix (and (memq major-mode company-mlton-modes)
                 (not (company-in-string-or-comment))
                 (or (company-mlton--prefix) 'stop)))
    (candidates (all-completions arg company-mlton-keyword--sml-keywords))
    (annotation "kw")
    (sorted 't)
    ))

;;;###autoload
(defun company-mlton-basis (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-mlton-basis))
    (prefix (and (memq major-mode company-mlton-modes)
                 company-mlton-basis--ids
                 (not (company-in-string-or-comment))
                 (or (company-mlton--prefix) 'stop)))
    (candidates (all-completions arg company-mlton-basis--ids))
    (annotation (get-text-property 0 'annotation arg))
    (meta (let ((meta (get-text-property 0 'meta arg)))
            (if company-echo-truncate-lines
                (replace-regexp-in-string "[ \n]+" " " meta)
              meta)))
    (location (get-text-property 0 'location arg))
    ))

;;;###autoload
(defun company-mlton-init ()
  (interactive)
  (set (make-local-variable 'company-backends) '((company-mlton-keyword company-mlton-basis)))
  (company-mode t))

(provide 'company-mlton)
;;; company-mlton.el ends here
