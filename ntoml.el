;;; ntoml.el --- Another TOML encoder / decoder -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (a "1.0.0"))
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(require 'a)

(defvar ntoml--current nil
  "Temporary storage.")

(defvar ntoml--current-location nil
  "Which table we should insert KV pairs to.

This is a list of keys.")

(defmacro ntoml-preserve-point-on-fail (&rest body)
  "Run BODY.

If it returns nil, go back to the point position before running
BODY."
  (declare (indent 0))
  `(let ((start (point)))
     (or (progn ,@body)
         (prog1 nil
           (goto-char start)))))

(defun ntoml-regexp-or (&rest regexps)
  "Return a regexp that matches any of REGEXPS."
  (mapconcat
   (lambda (r)
     (concat "\\(?:" r "\\)"))
   regexps
   "\\|"))

(defun ntoml-skip-forward-regexp (regexp &optional once)
  "If we're looking at REGEXP, skip to the end of it.

Return the number of characters skipped, or nil if we didn't
move."
  (let ((prev (point)))
    (if once
        (when (looking-at regexp)
          (goto-char (match-end 0)))
      (while (looking-at regexp)
        (goto-char (match-end 0))))
    (unless (= prev (point))
      (- (point) prev))))

(defun ntoml-skip-chars-forward (string &optional lim)
  "Like `skip-chars-forward', but return nil when we didn't move."
  (let ((v (skip-chars-forward string lim)))
    (unless (equal 0 v)
      v)))

(defmacro ntoml-skipped-region-allow-null (&rest body)
  "Run BODY, and return the text between the old and new point.

This will return an empty string if point hasn't moved."
  `(let (start end)
     (setq start (point))
     ,@body
     (setq end (point))
     ;; "They can be in either order."
     (buffer-substring-no-properties start end)))

(defmacro ntoml-skipped-region (&rest body)
  "Run BODY, and return the text between the old and new point.

Return nil if point hasn't moved."
  (declare (indent 0))
  `(let (start end)
     (setq start (point))
     ,@body
     (setq end (point))
     ;; "They can be in either order."
     (unless (equal start end)
       (buffer-substring-no-properties start end))))

(defun ntoml-encode (value)
  "Encode VALUE as TOML."
  (json-serialize value))

(defun ntoml-decode-buffer ()
  "Decode current buffer contents as TOML and return the Elisp value."
  (save-excursion
    (goto-char (point-min))
    (ntoml-read-toml)))

(defun ntoml-decode (toml-string)
  "Decode TOML-STRING into Elisp value."
  (with-temp-buffer
    (insert toml-string)
    (ntoml-decode-buffer)))

;;;; Base

(defconst ntoml--wschar "[\t ]")
(defconst ntoml--newline (rx (or "\n" "\x0d\n"))) ; LF or CRLF
(defconst ntoml--non-ascii (rx (in (#x80 . #x10FFFF))))
(defconst ntoml--non-eol (rx (or "\t"
                                 (in (#x20 . #x7F))
                                 (in (#x80 . #x10FFFF)))))

(defun ntoml-read-toml ()
  (setq ntoml--current nil
        ntoml--current-location nil)
  (ntoml-read-expression)
  (while (and (ntoml-read-newline)
              (ntoml-read-expression)))
  (prog1 ntoml--current
    (setq ntoml--current nil
          ntoml--current-location nil)))

(defun ntoml-read-expression ()
  (ntoml-skipped-region
    (ntoml-read-whitespace)
    (and (or (ntoml-read-keyval)
             (ntoml-read-table))
         (ntoml-read-whitespace))
    (ntoml-read-comment)))

(defun ntoml-read-whitespace ()
  (ntoml-skip-forward-regexp ntoml--wschar))

(defun ntoml-read-newline (&optional once)
  (ntoml-skip-forward-regexp ntoml--newline once))

(defun ntoml-read-comment ()
  (ntoml-skip-forward-regexp (format (rx "#" (* "%s"))
                                     ntoml--non-eol)))

(defun ntoml-read-ws/comment/newline ()
  "Skip through whitespace, comment, and newlines."
  (while (or (ntoml-read-whitespace)
             (ntoml-read-newline)
             (ntoml-preserve-point-on-fail
               (and (ntoml-read-comment)
                    (ntoml-read-newline))))))

;;;; KeyVal

(defun ntoml-read-keyval ()
  (let (k v)
    (when (and (setq k (ntoml-read-key))
               (ntoml-read-keyval-sep)
               (setq v (ntoml-read-val)))
      (setq ntoml--current
            (a-assoc-in ntoml--current
                        (append ntoml--current-location (list k))
                        v)))))

(defun ntoml-read-keyval-sep ()
  (ntoml-read-whitespace)
  (prog1 (ntoml-skip-chars-forward "=" (1+ (point)))
    (ntoml-read-whitespace)))

;;;; DONE Key

(defun ntoml-read-key ()
  (let (ret)
    (push (ntoml-read-simple-key) ret)
    (catch 'ret
      (while t
        (let ((prev (point))
              k)
          (if (and (ntoml-read-dot-sep)
                   (setq k (ntoml-read-simple-key)))
              (push k ret)
            (goto-char prev)
            (throw 'ret nil)))))
    (if (cdr ret)
        (nreverse ret)
      ;; Return first item if it is the only item (simple key)
      (car ret))))

(defun ntoml-read-simple-key ()
  (let ((v (or (ntoml-read-quoted-key)
               (ntoml-read-unquoted-key))))
    (when v
      (intern v))))
(defun ntoml-read-quoted-key ()
  (or (ntoml-read-basic-string)
      (ntoml-read-literal-string)))
(defun ntoml-read-unquoted-key ()
  (ntoml-skipped-region
    (ntoml-skip-forward-regexp (rx (+ (in "A-Za-z0-9_-"))))))
(defun ntoml-read-dot-sep ()
  (ntoml-read-whitespace)
  (prog1 (ntoml-skip-chars-forward "." (1+ (point)))
    (ntoml-read-whitespace)))

;;;; Val

(defun ntoml-read-val ()
  (or (ntoml-read-string)
      (ntoml-read-boolean)
      (ntoml-read-array)
      (ntoml-read-inline-table)
      ;; (ntoml-read-date-time)
      (ntoml-read-float)
      (ntoml-read-integer)))

;;;; String

(defun ntoml-read-string ()
  (or (ntoml-read-ml-basic-string)
      (ntoml-read-basic-string)
      (ntoml-read-ml-literal-string)
      (ntoml-read-literal-string)))

(defconst ntoml--basic-unescaped
  (string-join
   (list ntoml--wschar
         "!"
         "[\x23-\x5B]"
         "[\x5D-\x7E]"
         ntoml--non-ascii)
   "\\|"))
(defconst ntoml--escape "\\")
(defconst ntoml--escape-seq-char
  (rx (or "\""
          "\\"
          "b"
          "f"
          "n"
          "r"
          "t"
          (seq "u" (= 4 hex))
          (seq "U" (= 8 hex)))))
(defconst ntoml--escaped (format "%s\\(?:%s\\)"
                                 (regexp-quote ntoml--escape)
                                 ntoml--escape-seq-char))
(defconst ntoml--basic-char (ntoml-regexp-or ntoml--basic-unescaped ntoml--escaped))
(defconst ntoml--literal-char
  (format (rx (or "\t" (in "\x20-\x26") (in "\x28-\x7E") "%s"))
          ntoml--non-ascii))
(defconst ntoml--mlb-unescaped ntoml--basic-unescaped)
(defconst ntoml--mlb-char (ntoml-regexp-or ntoml--mlb-unescaped ntoml--escaped))
(defconst ntoml--mll-char
  (format (rx (or "\t" (in "\x20-\x26") (in "\x28-\x7E") "%s"))
          ntoml--non-ascii))
(defconst ntoml--mll-content
  (concat ntoml--mll-char "\\|" ntoml--newline))

(defun ntoml-read-basic-string ()
  (when (ntoml-skip-chars-forward "\"" (1+ (point)))
    (prog1 (ntoml-skipped-region-allow-null
            (ntoml-skip-forward-regexp ntoml--basic-char))
      (skip-chars-forward "\"" (1+ (point))))))

(defun ntoml-read-literal-string ()
  (when (ntoml-skip-chars-forward "'" (1+ (point)))
    (prog1 (ntoml-skipped-region-allow-null
            (ntoml-skip-forward-regexp ntoml--literal-char))
      (skip-chars-forward "'" (1+ (point))))))

(defun ntoml-read-ml-literal-string ()
  (when (ntoml-skip-forward-regexp "'''" :once)
    (ntoml-read-newline :once)
    (ntoml-read-ml-literal-body)
    (ntoml-skip-forward-regexp "'''" :once)))
(defun ntoml-read-ml-basic-string ()
  (when (ntoml-skip-forward-regexp "\"\"\"" :once)
    (ntoml-read-newline :once)
    (ntoml-read-ml-basic-body)
    (ntoml-skip-forward-regexp "\"\"\"" :once)))

(defun ntoml-read-mll-quotes ()
  (skip-chars-forward "'" (+ 2 (point))))
(defun ntoml-read-ml-literal-body ()
  (while (ntoml-skip-forward-regexp ntoml--mll-content))
  (while (and
          (ntoml-read-mll-quotes)
          (ntoml-skip-forward-regexp ntoml--mll-content)))
  (ntoml-read-mll-quotes))

(defun ntoml-read-mlb-quotes ()
  (skip-chars-forward "\"" (+ 2 (point))))
(defun ntoml-read-mlb-escaped-nl ()
  (ntoml-skip-forward-regexp (regexp-quote ntoml--escape) :once)
  (ntoml-read-whitespace)
  (ntoml-read-newline)
  (while (or (ntoml-read-whitespace)
             (ntoml-read-newline))))
(defun ntoml-read-mlb-content ()
  (or (ntoml-skip-forward-regexp ntoml--mlb-char)
      (ntoml-read-newline)
      (ntoml-read-mlb-escaped-nl)))
(defun ntoml-read-ml-basic-body ()
  (while (ntoml-read-mlb-content))
  (while (and
          (ntoml-read-mlb-quotes)
          (ntoml-read-mlb-content)))
  (ntoml-read-mlb-quotes))

;;;; DONE Integer

(defconst ntoml--unsigned-dec-int
  (rx (or (seq (in "1-9")
               (1+ (or digit (seq "_" digit))))
          ;; Try this second
          digit)))

(defconst ntoml--dec-int (rx (opt (any "+-"))
                             (regexp ntoml--unsigned-dec-int)))
(defconst ntoml--hex-int (concat "0x" (rx hex (* (or hex (seq "_" hex))))))
(defconst ntoml--oct-int (concat "0o" (rx (in "0-7") (* (or (in "0-7") (seq "_" (in "0-7")))))))
(defconst ntoml--bin-int (concat "0b" (rx (in "01") (* (or (in "01") (seq "_" (in "01")))))))

(define-error 'ntoml-integer-leading-zero "Integer has leading zero")

(defun ntoml-signal (sym data)
  (signal sym (append (list :message (get sym 'error-message))
                      data)))

(defun ntoml-read-integer ()
  (let ((value (ntoml-skipped-region
                 (or
                  (ntoml-skip-forward-regexp ntoml--hex-int)
                  (ntoml-skip-forward-regexp ntoml--oct-int)
                  (ntoml-skip-forward-regexp ntoml--bin-int)
                  (ntoml-skip-forward-regexp ntoml--dec-int)))))
    (when value
      (setq value (replace-regexp-in-string "_" "" value))
      (cond ((string-prefix-p "0x" value)
             (string-to-number (substring value 2) 16))
            ((string-prefix-p "0o" value)
             (string-to-number (substring value 2) 8))
            ((string-prefix-p "0b" value)
             (string-to-number (substring value 2) 2))
            (t
             (when (equal ?0 (elt value 0))
               (ntoml-signal 'ntoml-integer-leading-zero
                             (list :value value
                                   :point (point))))
             (string-to-number value))))))

;;;; DONE Float

(defconst ntoml--special-float (rx (group (opt (in "+-")))
                                   (group (or "inf" "nan"))))
(defconst ntoml--zero-prefixable-int (rx digit (* (or digit (seq "_" digit)))))
(defconst ntoml--frac (concat "\\." ntoml--zero-prefixable-int))
(defconst ntoml--float-exp-part (concat "[+-]" ntoml--zero-prefixable-int))
(defconst ntoml--float-int-part ntoml--dec-int)
(defconst ntoml--exp (concat "e" ntoml--float-exp-part))

(defun ntoml-read-float ()
  "Read a float."
  (let ((start (point)))
    (cond
     ((and (ntoml-skip-forward-regexp ntoml--float-int-part)
           (or (ntoml-skip-forward-regexp ntoml--exp)
               (cl-some #'identity
                        (list (ntoml-skip-forward-regexp ntoml--frac)
                              (ntoml-skip-forward-regexp ntoml--exp)))))
      (string-to-number (buffer-substring-no-properties start (point))))
     ((ntoml-skipped-region
        (goto-char start)
        (ntoml-skip-forward-regexp ntoml--special-float))
      (pcase (cons (match-string 1) (match-string 2))
        (`(,(or "" "+") . "inf") 1.0e+INF)
        (`(,(or "" "+") . "nan") 1.0e+NaN)
        (`("-" . "inf") -1.0e+INF)
        (`("-" . "nan") -1.0e+NaN)))
     (t
      (prog1 nil
        (goto-char start))))))

;;;; DONE Boolean

(defun ntoml-read-boolean ()
  "Read a boolean value."
  (pcase (ntoml-skipped-region
           (or (ntoml-skip-forward-regexp "true")
               (ntoml-skip-forward-regexp "false")))
    ("true" t)
    ;; TODO: :false-object like json-parse-string
    ("false" :false)))

;;;; TODO Date-Time (RFC 3339)

(defun ntoml-read-date-time ()
  (or (ntoml-read-offset-date-time)
      (ntoml-read-local-date-time)
      (ntoml-read-local-date)
      (ntoml-read-local-time)))

(defconst ntoml--time-delim (rx (or "T" "t" " ")))

(defun ntoml-read-full-date ())
(defun ntoml-read-partial-time ())
(defun ntoml-read-full-time ())

(defun ntoml-read-offset-date-time ()
  (ntoml-skipped-region
    (ntoml-read-full-date)
    (ntoml-skip-forward-regexp ntoml--time-delim)
    (ntoml-read-full-time)))

(defun ntoml-read-local-date-time ()
  (ntoml-skipped-region
    (ntoml-read-full-date)
    (ntoml-skip-forward-regexp ntoml--time-delim)
    (ntoml-read-partial-time)))

(defun ntoml-read-local-date ()
  (ntoml-skipped-region
    (ntoml-read-full-date)))

(defun ntoml-read-local-time ()
  (ntoml-skipped-region
    (ntoml-read-partial-time)))

;;;; Array

(defun ntoml-read-array ()
  (ntoml-preserve-point-on-fail
    (let (ret)
      (when (ntoml-skip-forward-regexp (rx "["))
        (setq ret (ntoml-read-array-values))
        (ntoml-read-ws/comment/newline)
        (ntoml-skip-forward-regexp (rx "]"))
        ret))))

(defun ntoml-read-array-values ()
  (cl-loop
   do (ntoml-read-ws/comment/newline)
   when (ntoml-read-val)
   collect it
   do (ntoml-read-ws/comment/newline)
   while (ntoml-skip-forward-regexp (rx ","))))

;;;; Table

(defun ntoml-read-table ()
  (or (ntoml-read-std-table)
      (ntoml-read-array-table)))

(defun ntoml-read-std-table ()
  (ntoml-preserve-point-on-fail
    (let (keys)
      (when (ntoml-skip-forward-regexp (rx "[") :once)
        (ntoml-read-whitespace)
        (setq keys (ntoml-read-key))
        (ntoml-read-whitespace)
        (when (ntoml-skip-forward-regexp (rx "]") :once)
          (when keys
            (unless (listp keys)
              (setq keys (list keys)))
            (setq ntoml--current-location keys)
            (unless (a-get-in ntoml--current keys)
              (setq ntoml--current (a-assoc-in ntoml--current keys nil))))
          t)))))

(defun ntoml-read-inline-table ()
  (ntoml-preserve-point-on-fail
    (let (ret)
      (when (ntoml-skip-forward-regexp (rx "{"))
        (ntoml-read-whitespace)
        (setq ret (ntoml-read-inline-table-keyvals))
        (ntoml-read-whitespace)
        (when (ntoml-skip-forward-regexp (rx "}"))
          ret)))))

(defun ntoml-read-inline-table-keyvals ()
  (cl-loop when (ntoml-read-keyval)
           collect it
           while (prog2
                     (ntoml-read-whitespace)
                     (ntoml-skip-forward-regexp (rx ","))
                   (ntoml-read-whitespace))))

(defun ntoml-read-array-table ()
  (when (ntoml-skip-forward-regexp (rx "[["))
    (ntoml-read-whitespace)
    (ntoml-read-key)
    (ntoml-read-whitespace)
    (ntoml-skip-forward-regexp (rx "]]"))))

;; Inline Table
;; Array Table

(provide 'ntoml)

;;; ntoml.el ends here
