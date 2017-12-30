;;; company-mlton.el --- company-mode backend for MLton/Standard ML  -*- lexical-binding: t -*-

;; Copyright (C) 2017  Matthew Fluet

;; Author: Matthew Fluet <Matthew.Fluet@gmail.com>
;; URL: https://github.com/MatthewFluet/company-mlton
;; Version: 1.0
;; Keywords: company-mode mlton standard-ml
;; Package-Requires: ((emacs "24.4") (company "0.9.4") (dash "2.13.0"))

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

(require 'pcase)
(require 'company)
(require 'dash)

(defconst company-mlton--dir
  (file-name-directory load-file-name))

;; company-mlton customization

(defgroup company-mlton nil
  "Completion backend for MLton/Standard ML."
  :group 'company)

(defcustom company-mlton-modes '(sml-mode)
  "Major modes in which company-mlton may complete."
  :group 'company-mlton)

(defcustom company-mlton-verbose t
  "Whether to echo messages that are not errors."
  :group 'company-mlton-basis
  :type 'boolean)

;; company-mlton regexps

(defun company-mlton--rev-rx (rx)
  "Returns an `rx' sexp that accepts the language of reversed words
accepted by the `rx' sexp RX.

Only handles a small subset of `rx' sexp forms."
  (pcase rx
    ((pred stringp) rx)
    ((pred characterp) rx)
    (`(char . ,rest) rx)
    (`(: . ,rest) (cons `: (reverse (-map #'company-mlton--rev-rx rest))))
    (`(| . ,rest) (cons `| (-map #'company-mlton--rev-rx rest)))
    (`(* . ,rest) (cons `* (-map #'company-mlton--rev-rx rest)))
    (`(+ . ,rest) (cons `+ (-map #'company-mlton--rev-rx rest)))
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
  `(? (: " " (| ,company-mlton--sml-tyvar-id-rx
                (: "(" ,company-mlton--sml-tyvar-id-rx
                   (* (: "," " " ,company-mlton--sml-tyvar-id-rx)) ")")))))
(defconst company-mlton--sml-tyvars-re
  (rx-to-string company-mlton--sml-tyvars-rx))


;; company-mlton utils

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
  "If point is at the end of a prefix of an SML long identifier,
return it.
If point is in the middle of a prefix of an SML long identifier,
return 'stop.
Otherwise, return 'nil."
  (let ((rev-pre-line (string-reverse
                       (buffer-substring-no-properties (point-at-bol) (point)))))
    (when (string-match
           company-mlton--rev-prefix-sml-long-id-at-start-re
           rev-pre-line)
      (let ((prefix (string-reverse
                     (match-string-no-properties 0 rev-pre-line))))
        ;; match must succeed
        (string-match
         company-mlton--prefix-sml-long-id-at-start-re
         (concat prefix (buffer-substring-no-properties (point) (point-at-eol))))
        (if (= (length prefix) (match-end 0))
            prefix
          'stop)))))


;; company-mlton-keyword

(defconst company-mlton-keyword--sml-keywords-core
  '("abstype" "and" "andalso" "as" "case" "datatype" "do" "else"
    "end" "exception" "fn" "fun" "handle" "if" "in" "infix"
    "infixr" "let" "local" "nonfix" "of" "op" "open" "orelse"
    "raise" "rec" "then" "type" "val" "with" "withtype" "while")
  "The list of Standard ML keywords for the core language.")
(defconst company-mlton-keyword--sml-keywords-modules
  '("eqtype" "functor" "include" "sharing" "sig"
    "signature" "struct" "structure" "where")
  "The list of Standard ML keywords for the modules language.")
(defconst company-mlton-keyword--sml-keywords
  (sort (append company-mlton-keyword--sml-keywords-core
                company-mlton-keyword--sml-keywords-modules)
        'string<)
  "The list of Standard ML keywords.")

;;;###autoload
(defun company-mlton-keyword (command &optional arg &rest ignored)
  "`company-mode' completion backend for Standard ML keywords."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-mlton-keyword))
    (`prefix (and (memq major-mode company-mlton-modes)
                  (not (company-in-string-or-comment))
                  (or (company-mlton--prefix) 'stop)))
    (`candidates (all-completions arg company-mlton-keyword--sml-keywords))
    (`annotation "kw")
    (`sorted 't)))


;; company-mlton-basis

(defconst company-mlton-basis--entry-rx
  `(: line-start
      (| (: (group-n 2 (| "type" "datatype")) ,company-mlton--sml-tyvars-rx
            " " (group-n 1 ,company-mlton--sml-long-id-rx))
         (: (group-n 2 (| "con" "exn" "val" "signature" "structure" "functor"))
            " " (group-n 1 ,company-mlton--sml-long-id-rx)))
      (* not-newline) "\n"
      (* " " (* not-newline) "\n"))
  "An `rx' sexp to match entries in a basis file.

Group #1 matches the SML long identifier.
Group #2 matches the SML keyword.")
(defconst company-mlton-basis--entry-re
  (rx-to-string company-mlton-basis--entry-rx)
  "A regexp to match entries in a basis file.

Group #1 matches the SML long identifier.
Group #2 matches the SML keyword.")
(defconst company-mlton-basis--entry-def-rx
  `(: "(* @ "
      (group-n 1 (* (not (any " "))))
      " "
      (group-n 2 (+ digit)) "." (+ digit)
      (? (: "-" (+ digit) "." (+ digit)))
      " *)")
  "An `rx' sexp to match (non-bogus) definition locations in a basis file.

Group #1 matches the file name.
Group #2 matches the line number.")
(defconst company-mlton-basis--entry-def-re
  (rx-to-string company-mlton-basis--entry-def-rx)
  "A regexp to match (non-bogus) definition locations in a basis file.

Group #1 matches the file name.
Group #2 matches the line number.")

(defun company-mlton-basis--load-ids-from-file (file)
  "Load identifiers described by a basis file FILE.

Returns a list of strings.  Each string corresponds to an
identifier from one entry in the basis file FILE.  The annotation
text property of the string corresponds to the identifier
class (one of \"typ\", \"con\", \"exn\", \"val\", \"sig\",
\"str\", or \"fct\").  The meta text property of string
corresponds to the whole entry, excepting the definition
location.  The location property of the string corresponds to the
definition location of the identifier."
  (when company-mlton-verbose
    (message "company-mlton loading file \"%s\"" file))
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((ids nil))
      (while (re-search-forward company-mlton-basis--entry-re nil t)
        (let* ((entry (match-string-no-properties 0))
               (id (match-string-no-properties 1))
               (kw (match-string-no-properties 2))
               (annotation (pcase (substring kw 0 3)
                             ("dat" "typ")
                             ("fun" "fct")
                             (ann ann)))
               (meta (string-trim-right
                      (replace-regexp-in-string
                       "(\\* @.*\\*)" ""
                       entry)))
               (location (when (string-match company-mlton-basis--entry-def-re entry)
                           (cons (match-string-no-properties 1 entry)
                                 (string-to-number (match-string-no-properties 2 entry))))))
          (push (propertize id
                            'annotation annotation
                            'meta meta
                            'location location)
                ids)))
      ids)))

(defconst company-mlton-basis-file--standard
  (expand-file-name "mlton-default.basis" company-mlton--dir)
  "The standard value for the (buffer-local) variable `company-mlton-basis-file'.

Corresponds to MLton's default environment (implicitly used when
compiling a \".sml\" file).")

(defcustom company-mlton-basis-file company-mlton-basis-file--standard
  "The basis file associated with the current buffer."
  :group 'company-mlton
  :type '(file)
  :safe #'string-or-null-p)
(make-variable-buffer-local 'company-mlton-basis-file)

(defvar company-mlton-basis--cache-hash-table
  (make-hash-table :test 'equal :weakness 'value)
  "A global value-weak hash-table for mapping basis files to ids.

The hash-table maps (expanded) file names to caches of the
following forms:
  (FILE 'not-loaded)
  (FILE 'not-readable)
  (FILE 'loaded TIME IDS)
where FILE is the expanded file name, 'not-loaded indicates that
the FILE has never been loaded, 'not-readable indicates that the
FILE was not readable at the last attempt to load it (and,
therefore, subsequent not-readable error messages should be
suppressed), 'loaded indicates that the FILE was successfully
loaded, TIME is the file modification time when FILE was loaded,
and IDS is the list of identifiers loaded from FILE.

The hash-table is value-weak to allow caches (particularly
(FILE 'loaded TIME IDS) caches) to be garbage collected when
there are no buffers requiring that basis file.")

(defvar-local company-mlton-basis--cache nil
  "The cache associated with the current buffer.

See `company-mlton-basis--cache-hash-table' for a description of
cache forms.

This buffer-local variable is a strong reference to the cache,
maintaining it in the value-weak hash-table.  Moreover, when the
cache corresponds to `company-mlton-basis-file', then a
hash-table lookup is not required.")

(defun company-mlton-basis--fetch-ids ()
  "Fetch identifiers from the basis file specified by
`company-mlton-basis-file', using and updating cache as
necessary."
  (-when-let (file company-mlton-basis-file)
    (let* ((kfile (expand-file-name file))
           (cache
            (if (and company-mlton-basis--cache
                     (string-equal (car company-mlton-basis--cache) kfile))
                company-mlton-basis--cache
              (or (gethash kfile company-mlton-basis--cache-hash-table)
                  (let ((cache (list kfile 'not-loaded)))
                    (setq company-mlton-basis--cache cache)
                    (puthash kfile cache company-mlton-basis--cache-hash-table)
                    cache))))
           (kfile-time (-if-let (attrs (file-attributes kfile)) (nth 5 attrs)))
           (load-ids
            (lambda (show-unreadable-message)
              (if (not (file-readable-p kfile))
                  (progn
                    (when show-unreadable-message
                      (message "company-mlton could not read file \"%s\""
                               kfile))
                    (setcdr cache (list 'not-readable))
                    nil)
                (let ((ids (company-mlton-basis--load-ids-from-file kfile)))
                  (when (null ids)
                    (message "company-mlton found no identifiers in file \"%s\""
                             kfile))
                  (setcdr cache (list 'loaded kfile-time ids))
                  ids)))))
      (pcase (cadr cache)
        (`not-loaded (funcall load-ids t))
        (`not-readable (funcall load-ids nil))
        (`loaded (if (time-less-p (caddr cache) kfile-time)
                     (funcall load-ids t)
                   (cadddr cache)))))))

;;;###autoload
(defun company-mlton-basis-load (file)
  "Load a basis file FILE created by 'mlton' using '-show-basis <file>' or '(*#showBasis \"<file>\"*)'."
  (interactive "fBasis file: ")
  (setq company-mlton-basis-file file)
  (company-mlton-basis--fetch-ids)
  nil)

(defconst company-mlton-basis--show-basis-directive-rx
  `(: "(*#showBasis" (+ " ") "\"" (group-n 1 (* (not (any "\"")))) "\"" (* " ") "*)")
  "An `rx' sexp to match '(*#showBasis \"<file>\"*)' directives.

Group #1 matches the file name.")
(defconst company-mlton-basis--show-basis-directive-re
  (rx-to-string company-mlton-basis--show-basis-directive-rx)
  "A regexp to match '(*#showBasis \"<file>\"*)' directives.

Group #1 matches the file name.")

;;;###autoload
(defun company-mlton-basis-autodetect ()
  "Attempt to autodetect the basis file for use with the current buffer.

Currently, searches the current buffer for
'(*#showBasis \"<file>\"*)' directives and sets
`company-mlton-basis-file'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward company-mlton-basis--show-basis-directive-re nil t)
      (setq company-mlton-basis-file (match-string-no-properties 1)))))

;;;###autoload
(defun company-mlton-basis (command &optional arg &rest ignored)
  "`company-mode' completion backend for Standard ML identifiers.

Candidate completion identifiers are loaded from a basis file
created by 'mlton' using '-show-basis <file>' or
'(*#showBasis \"<file>\"*)' and specified by the (buffer-local)
variable `company-mlton-basis-file'."
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-mlton-basis))
    (`prefix (and (memq major-mode company-mlton-modes)
                  company-mlton-basis-file
                  (not (company-in-string-or-comment))
                  (or (company-mlton--prefix) 'stop)))
    (`candidates (all-completions arg (company-mlton-basis--fetch-ids)))
    (`annotation (get-text-property 0 'annotation arg))
    (`meta (let ((meta (get-text-property 0 'meta arg)))
             (if company-echo-truncate-lines
                 (replace-regexp-in-string "[ \n]+" " " meta)
               (let ((metas (split-string meta "\n"))
                     (max-lines (if (integerp max-mini-window-height)
                                    max-mini-window-height
                                  (truncate (* max-mini-window-height
                                               (frame-total-lines))))))
                 (if (<= (length metas) max-lines)
                     meta
                   (string-join (-take max-lines metas) "\n"))))))
    (`location (-when-let (file_line (get-text-property 0 'location arg))
                 (when (file-readable-p (car file_line))
                   file_line)))))


;; company-mlton-init

(defconst company-mlton-grouped-backend
  (list #'company-mlton-keyword #'company-mlton-basis)
  "A grouped backed for `company-backends' consisting of
`company-mlton-keyword' and `company-mlton-basis'.")

;;;###autoload
(defun company-mlton-init ()
  "Init company-mlton backend.

Makes variable `company-backends' buffer local, pushes
`company-mlton-grouped-backend' (consisting of
`company-mlton-keyword' and `company-mlton-basis') to it, calls
`company-mlton-basis-autodetect', and enables `company-mode'.

This function is suitable for adding to `sml-mode-hook'."
  (interactive)
  (make-local-variable 'company-backends)
  (push company-mlton-grouped-backend company-backends)
  (company-mlton-basis-autodetect)
  (company-mode t))

(provide 'company-mlton)
;;; company-mlton.el ends here
