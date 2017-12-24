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
(require 'dash)

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

(defun company-mlton--rev-rx (rx)
  (pcase rx
    ((pred stringp) rx)
    ((pred characterp) rx)
    (`(char . ,rest) rx)
    (`(: . ,rest) (cons `: (reverse (-map #'company-mlton--rev-rx rest))))
    (`(| . ,rest) (cons `| (-map #'company-mlton--rev-rx rest)))
    (`(* . ,rest) (cons `* (-map #'company-mlton--rev-rx rest)))
    (`(+ . ,rest) (cons `* (-map #'company-mlton--rev-rx rest)))
    (`(? . ,rest) (cons `? (-map #'company-mlton--rev-rx rest)))))

(defconst company-mlton--sml-alphanum-rx
  `(char "A-Z" "a-z" "0-9" "'" "_"))
(defconst company-mlton--sml-alphanum-id-rx
  `(: (char "A-Z" "a-z") (* ,company-mlton--sml-alphanum-rx)))
(defconst company-mlton--sml-sym-rx
  `(char "!" "%" "&" "$" "#" "+" "-"
         "/" ":" "<" "=" ">" "?" "@"
         "\\" "~" "`" "^" "|" "*"))
(defconst company-mlton--sml-sym-id-rx
  `(+ ,company-mlton--sml-sym-rx))
(defconst company-mlton--sml-long-id-rx
  `(: (* (: ,company-mlton--sml-alphanum-id-rx "."))
      (| ,company-mlton--sml-alphanum-id-rx
         ,company-mlton--sml-sym-id-rx)))
(defconst company-mlton--sml-long-id-re
  (rx-to-string company-mlton--sml-long-id-rx))
(defconst company-mlton--prefix-sml-long-id-rx
  `(: (* (: ,company-mlton--sml-alphanum-id-rx "."))
      (| (: ,company-mlton--sml-alphanum-id-rx ".")
         ,company-mlton--sml-alphanum-id-rx
         ,company-mlton--sml-sym-id-rx)))
(defconst company-mlton--prefix-sml-long-id-at-start-rx
  `(: string-start ,company-mlton--prefix-sml-long-id-rx))
(defconst company-mlton--prefix-sml-long-id-at-start-re
  (rx-to-string company-mlton--prefix-sml-long-id-at-start-rx))
(defconst company-mlton--rev-prefix-sml-long-id-at-start-rx
  `(: string-start ,(company-mlton--rev-rx company-mlton--prefix-sml-long-id-rx)))
(defconst company-mlton--rev-prefix-sml-long-id-at-start-re
  (rx-to-string company-mlton--rev-prefix-sml-long-id-at-start-rx))

(defconst company-mlton--sml-tyvar-id-rx
  `(: "'" (* ,company-mlton--sml-alphanum-rx)))
(defconst company-mlton--sml-tyvars-rx
  `(? (| (: ,company-mlton--sml-tyvar-id-rx)
         (: "(" ,company-mlton--sml-tyvar-id-rx 
            (* (: "," " " ,company-mlton--sml-tyvar-id-rx)) ")"))))
(defconst company-mlton--sml-tyvars-re
  (rx-to-string company-mlton--sml-tyvars-rx))


;; Robustly match SML long identifier prefixes.
;;
;; Many company backends use `company-grab-symbol` or
;; `company-grab-word`.  These functions rely on robust syntax tables
;; for symbol and word boundaries.  However, old versions of sml-mode
;; (e.g., the modified sml-mode-3.3 that I (Matthew Fluet) use) have
;; poor syntax tables and neither `company-grab-symbol` nor
;; `company-grab-word` return a prefix that includes "." (i.e., a
;; proper long identifier).  Recent versions of sml-mode (e.g., Stefan
;; Monnier's sml-mode-6.8 via elpa) have better syntax tables, and
;; `company-grab-symbol` works for alphanumeric long identifiers, but
;; not for symbolic long identifiers (e.g., "Int.<=").
;;
;; Consider the line "1+IntInf.di" with the point at the end.
;; `company-mlton--prefix` should return "IntInf.di".
;; `(re-search-backward prefix-sml-long-id-re)` would only match "i".
;; `(looking-back prefix-sml-long-id-re)` would also only match "i";
;; moreover, `(looking-back prefix-sml-long-id-re nil t)` would only
;; match "di", because ".di" does not match `prefix-sml-long-id-re`.
;; Skipping backward through alphanumeric and symbolic and "."
;; characters would return "+IntInf.di".  We can match "IntInf.di" by
;; taking the longest match of `rev-prefix-sml-long-id-re` in the
;; reversed string "id.fnItnI+1".  Having found the beginning of the
;; long identifier that includes the point, we take the longest match
;; of `prefix-sml-long-id-re`.  If this match ends at the point, then
;; the point is at the end of a prefix of an SML long identifier; if
;; this match ends after the point, then the point is in the middle of
;; a prefix of an SML long identifier.
;;
;; Unfortunately, there does not appear to be a way to regex search
;; through the buffer in reverse (i.e., search for a regex match in
;; the sequence of characters backwards from the point).  We
;; explicitly construct the reversed prefix of the current line (which
;; suffices for finding a prefix of an SML long identifier), take the
;; longest match of `rev-prefix-sml-long-id-at-start-re`, explicitly
;; construct the matched prefix with the suffix of the current line,
;; and compare the length of the longest match of
;; `prefix-sml-long-id-at-start-re`.
(defun company-mlton--prefix ()
  "If point is at the end of a prefix of an SML long identifier, return it.
If point is in the middle of a prefix of an SML long identifier, return 'stop.
Otherwise, return 'nil."
  (let ((rev-pre-line (reverse (buffer-substring (point-at-bol) (point)))))
    (when (string-match
           company-mlton--rev-prefix-sml-long-id-at-start-re
           rev-pre-line)
      (let ((prefix (reverse (match-string 0 rev-pre-line))))
        ;; match must succeed
        (string-match
         company-mlton--prefix-sml-long-id-at-start-re
         (concat prefix (buffer-substring (point) (point-at-eol))))
        (if (= (length prefix) (match-end 0))
            prefix
          'stop)))))

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
    (location (-when-let (file_line (get-text-property 0 'location arg))
                (when (file-readable-p (car file_line))
                  file_line)))
    ))

;;;###autoload
(defun company-mlton-init ()
  (interactive)
  (set (make-local-variable 'company-backends) '((company-mlton-keyword company-mlton-basis)))
  (company-mode t))

(provide 'company-mlton)
;;; company-mlton.el ends here
