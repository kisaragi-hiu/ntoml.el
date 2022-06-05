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

(defvar ntoml--current-year nil
  "When reading a datetime, the year to calculate bounds with.")
(defvar ntoml--current-month nil
  "When reading a datetime, the month to calculate bounds with.")
(defvar ntoml--current nil
  "Temporary storage.")
(defvar ntoml--current-location nil
  "Which table we should insert KV pairs to.

This is a list of keys.")
(defvar ntoml--reading-array-table nil
  "Whether we're inserting into an array table.")
(defvar ntoml--array-table-pending nil
  "Value to be inserted into the current table.")
(defvar ntoml--seen-keys nil
  "List of keys that we've seen.")

(defmacro ntoml-preserve-point-on-fail (&rest body)
  "Run BODY.

If it returns nil, go back to the point position before running
BODY."
  (declare (indent 0))
  `(let ((start (point)))
     (or (progn ,@body)
         (prog1 nil
           (goto-char start)))))

(defun ntoml-signal (sym &optional value)
  (let ((data (list :message (get sym 'error-message)
                    :point (point))))
    (signal sym
            (if value
                (plist-put data :value value)
              data))))

(defun ntoml-eolp ()
  "Are we at the end of line (ignoring whitespace and comment)?"
  (ntoml-read-whitespace)
  (ntoml-read-comment)
  (eolp))

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

(cl-defgeneric ntoml-encode (value)
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

(defun ntoml-reset-state ()
  (setq ntoml--current nil
        ntoml--current-location nil
        ntoml--reading-array-table nil
        ntoml--array-table-pending nil
        ntoml--seen-keys nil
        ntoml--current-year nil
        ntoml--current-month nil))

(defun ntoml-read-toml ()
  (ntoml-reset-state)
  (ntoml-read-expression)
  (while (and (ntoml-read-newline)
              (ntoml-read-expression)))
  (ntoml-array-table-flush)
  (prog1 (nreverse ntoml--current)
    (ntoml-reset-state)))

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

(define-error 'ntoml-keyval-invalid "Invalid key-value pair")
(define-error 'ntoml-keyval-trailing-garbage "Trailing garbage after key-value pair")

(defun ntoml-read-keyval (&optional inline)
  "Read a key-value pair.

If INLINE is non-nil, don't signal an error when there is text
following the pair and don't touch `ntoml--current'."
  (let (k v)
    (when (and (setq k (ntoml-read-key))
               (ntoml-read-keyval-sep))
      (setq v (ntoml-read-val))
      (unless (or inline (ntoml-eolp))
        (ntoml-signal 'ntoml-keyval-trailing-garbage))
      (unless v
        (ntoml-signal 'ntoml-keyval-invalid))
      (if inline
          (cons k
                (cond ((eq v :empty) nil)
                      (t v)))
        (if ntoml--reading-array-table
            (push (cons k v) ntoml--array-table-pending)
          (setq ntoml--current
                (a-assoc-in ntoml--current
                            (append ntoml--current-location (list k))
                            v)))))))

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
  (or
   (ntoml-read-date-time)
   (ntoml-read-string)
   (ntoml-read-boolean)
   (ntoml-read-array)
   (ntoml-read-inline-table)
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

(define-error 'ntoml-string-not-closed "String not closed properly")

(defun ntoml-read-basic-string ()
  (when (ntoml-skip-chars-forward "\"" (1+ (point)))
    (prog1 (ntoml-skipped-region-allow-null
            (ntoml-skip-forward-regexp ntoml--basic-char))
      (unless (ntoml-skip-chars-forward "\"" (1+ (point)))
        (ntoml-signal 'ntoml-string-not-closed)))))

(defun ntoml-read-literal-string ()
  (when (ntoml-skip-chars-forward "'" (1+ (point)))
    (prog1 (ntoml-skipped-region-allow-null
            (ntoml-skip-forward-regexp ntoml--literal-char))
      (unless (ntoml-skip-chars-forward "'" (1+ (point)))
        (ntoml-signal 'ntoml-string-not-closed)))))

(defun ntoml-read-ml-literal-string ()
  (when (ntoml-skip-forward-regexp "'''" :once)
    (ntoml-read-newline :once)
    (prog1 (ntoml-skipped-region-allow-null
            (ntoml-read-ml-literal-body))
      (unless (ntoml-skip-forward-regexp "'''" :once)
        (ntoml-signal 'ntoml-string-not-closed)))))

(defun ntoml-read-ml-basic-string ()
  (when (ntoml-skip-forward-regexp "\"\"\"" :once)
    (ntoml-read-newline :once)
    (prog1 (ntoml-skipped-region-allow-null
            (ntoml-read-ml-basic-body))
      (unless (ntoml-skip-forward-regexp "\"\"\"" :once)
        (ntoml-signal 'ntoml-string-not-closed)))))

(defun ntoml-read-mll-quotes ()
  (ntoml-skip-forward-regexp (rx (repeat 1 2 "'")) :once))
(defun ntoml-read-ml-literal-body ()
  (while (ntoml-skip-forward-regexp ntoml--mll-content))
  (when (ntoml-preserve-point-on-fail
          (and
           (ntoml-read-mll-quotes)
           (ntoml-skip-forward-regexp ntoml--mll-content)))
    (ntoml-read-mll-quotes)))

(defun ntoml-read-mlb-quotes ()
  (ntoml-skip-forward-regexp (rx (repeat 1 2 "\"")) :once))

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
  (when (ntoml-preserve-point-on-fail
          (and
           (ntoml-read-mlb-quotes)
           (ntoml-read-mlb-content)))
    (ntoml-read-mlb-quotes)))

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
                             value))
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

(cl-defstruct (ntoml-date-time
               (:copier nil)
               (:constructor ntoml-date-time))
  timestamp)

(cl-defstruct (ntoml-date-time-local
               (:copier nil)
               (:constructor ntoml-date-time-local))
  timestamp)

(cl-defstruct (ntoml-date-local
               (:copier nil)
               (:constructor ntoml-date-local))
  timestamp)

(cl-defstruct (ntoml-time-local
               (:copier nil)
               (:constructor ntoml-time-local))
  timestamp)

(cl-defmethod ntoml-encode ((datetime ntoml-date-time))
  (ntoml-date-time-timestamp datetime))

(defun ntoml-read-date-time ()
  (let* ((type nil)
         (val (ntoml-skipped-region
                (or (and (ntoml-read-offset-date-time)
                         (setq type 'date-time))
                    (and (ntoml-read-local-date-time)
                         (setq type 'date-time-local))
                    (and (ntoml-read-local-date)
                         (setq type 'date-local))
                    (and (ntoml-read-local-time)
                         (setq type 'time-local))))))
    (when type
      (pcase type
        ('date-time (ntoml-date-time :timestamp val))
        ('date-time-local (ntoml-date-time-local :timestamp val))
        ('date-local (ntoml-date-local :timestamp val))
        ('time-local (ntoml-time-local :timestamp val))))))

(define-error 'ntoml-date-time-invalid "Invalid date time")
(define-error 'ntoml-date-month-invalid "Invalid month")
(define-error 'ntoml-date-mday-invalid "Invalid month day")
(define-error 'ntoml-time-hour-invalid "Invalid hour")
(define-error 'ntoml-time-minute-invalid "Invalid minute")
(define-error 'ntoml-time-second-invalid "Invalid second")

(defconst ntoml--time-delim (rx (or "T" "t" " ")))

(defun ntoml-read-date-fullyear ()
  (when (ntoml-skip-forward-regexp (rx (= 4 digit)) :once)
    (let ((ret (string-to-number (match-string 0))))
      (setq ntoml--current-year ret)
      ret)))
(defun ntoml-read-date-month ()
  (when (ntoml-skip-forward-regexp (rx (= 2 digit)) :once)
    (let ((ret (string-to-number (match-string 0))))
      (unless (<= 1 ret 12)
        (ntoml-signal 'ntoml-date-month-invalid ret))
      (setq ntoml--current-month ret)
      ret)))
(defun ntoml-read-date-mday ()
  (when (ntoml-skip-forward-regexp (rx (= 2 digit)) :once)
    (let ((ret (string-to-number (match-string 0))))
      (unless (<= 1
                  ret
                  (cl-case ntoml--current-month
                    ((1 3 5 7 8 10 12) 31)
                    ((4 6 9 11) 31)
                    (t
                     (if (date-leap-year-p ntoml--current-year)
                         29
                       28))))
        (ntoml-signal 'ntoml-date-mday-invalid ret))
      (setq ntoml--current-month nil
            ntoml--current-year nil)
      ret)))
(defun ntoml-read-time-hour ()
  (when (ntoml-skip-forward-regexp (rx (= 2 digit)) :once)
    (let ((ret (string-to-number (match-string 0))))
      (unless (<= 0 ret 23)
        (ntoml-signal 'ntoml-time-hour-invalid ret))
      ret)))
(defun ntoml-read-time-minute ()
  (when (ntoml-skip-forward-regexp (rx (= 2 digit)) :once)
    (let ((ret (string-to-number (match-string 0))))
      (unless (<= 0 ret 59)
        (ntoml-signal 'ntoml-time-minute-invalid ret))
      ret)))
(defun ntoml-read-time-second ()
  (when (ntoml-skip-forward-regexp (rx (= 2 digit)) :once)
    (let ((ret (string-to-number (match-string 0))))
      (unless (<= 0 ret 60) ; leap second
        (ntoml-signal 'ntoml-time-second-invalid ret))
      ret)))
(defun ntoml-read-time-secfrac ()
  (ntoml-skipped-region
    (ntoml-skip-forward-regexp (rx "." (+ digit)) :once)))
(defun ntoml-read-time-numoffset ()
  (and (ntoml-skip-forward-regexp "[+-]" :once)
       (ntoml-read-time-hour)
       (ntoml-skip-chars-forward ":" (1+ (point)))
       (ntoml-read-time-minute)))
(defun ntoml-read-time-offset ()
  (or (ntoml-preserve-point-on-fail
        (ntoml-skip-forward-regexp "Z" :once))
      (ntoml-preserve-point-on-fail
        (ntoml-read-time-numoffset))))

(defun ntoml-read-partial-time ()
  (let (h m s frac)
    (when (and (setq h (ntoml-read-time-hour))
               (ntoml-skip-chars-forward ":" (1+ (point)))
               (setq m (ntoml-read-time-minute))
               (ntoml-skip-chars-forward ":" (1+ (point)))
               (setq s (ntoml-read-time-second)))
      (setq frac (ntoml-read-time-secfrac)))
    (if frac
        (list h m s frac)
      (list h m s frac))))
(defun ntoml-read-full-date ()
  (let (y m d)
    (when (and (setq y (ntoml-read-date-fullyear))
               (ntoml-skip-chars-forward "-" (1+ (point)))
               (setq m (ntoml-read-date-month))
               (ntoml-skip-chars-forward "-" (1+ (point)))
               (setq d (ntoml-read-date-mday)))
      (list y m d))))
(defun ntoml-read-full-time ()
  (ntoml-preserve-point-on-fail
    (and (ntoml-read-partial-time)
         (ntoml-read-time-offset))))
(defun ntoml-read-offset-date-time ()
  (ntoml-preserve-point-on-fail
    (and (ntoml-read-full-date)
         (ntoml-skip-forward-regexp ntoml--time-delim :once)
         (ntoml-read-full-time))))
(defun ntoml-read-local-date-time ()
  (ntoml-preserve-point-on-fail
    (and (ntoml-read-full-date)
         (ntoml-skip-forward-regexp ntoml--time-delim :once)
         (ntoml-read-partial-time))))
(defun ntoml-read-local-date ()
  (ntoml-skipped-region
    (ntoml-read-full-date)))
(defun ntoml-read-local-time ()
  (ntoml-skipped-region
    (ntoml-read-partial-time)))

;;;; Array

(define-error 'ntoml-array-not-closed "Array not closed properly")

(defun ntoml-read-array ()
  (ntoml-preserve-point-on-fail
    (let (ret)
      (when (ntoml-skip-forward-regexp (rx "[") :once)
        (setq ret (ntoml-read-array-values))
        (ntoml-read-ws/comment/newline)
        (unless (ntoml-skip-forward-regexp (rx "]") :once)
          (ntoml-signal 'ntoml-array-not-closed))
        (vconcat ret)))))

(defun ntoml-read-array-values ()
  (cl-loop
   do (ntoml-read-ws/comment/newline)
   when (ntoml-read-val)
   collect it
   do (ntoml-read-ws/comment/newline)
   while (ntoml-skip-forward-regexp (rx ","))))

;;;; Table

(define-error 'ntoml-table-key-invalid "Invalid table key")
(define-error 'ntoml-table-trailing-garbage "Trailing garbage after table key")
(define-error 'ntoml-table-redefine "Tried to use non-table key as table")
(define-error 'ntoml-table-duplicate "Duplicate key")

(defun ntoml-array-table-flush ()
  (when ntoml--reading-array-table
    (let ((current-array (a-get-in ntoml--current ntoml--current-location)))
      (setq
       ntoml--current
       (a-assoc-in
        ntoml--current
        ntoml--current-location
        (vconcat
         current-array
         (list (reverse ntoml--array-table-pending))))))
    (setq ntoml--array-table-pending nil)))

(defun ntoml-read-table ()
  (prog1 (or (ntoml-read-array-table)
             (ntoml-read-std-table))
    (ntoml-read-whitespace)
    (ntoml-read-comment)
    (unless (ntoml-eolp)
      (ntoml-signal 'ntoml-table-trailing-garbage))))

(defun ntoml-read-std-table ()
  "Read a standard table.

On success:

- store keys into `ntoml--current-location'
- disable `ntoml--reading-array-table'
- ensure keys exists in `ntoml--current'
- return keys

When we're not reading a standard table, return nil.

When we're reading something invalid, signal an error."
  (ntoml-preserve-point-on-fail
    (let (keys)
      (when (ntoml-skip-forward-regexp (rx "[") :once)
        (ntoml-read-whitespace)
        (setq keys (ntoml-read-key))
        (ntoml-read-whitespace)
        (if (ntoml-skip-forward-regexp (rx "]") :once)
            (when keys
              (ntoml-array-table-flush)
              (setq ntoml--reading-array-table nil)
              (unless (listp keys)
                (setq keys (list keys)))
              (when (member keys ntoml--seen-keys)
                (ntoml-signal 'ntoml-table-duplicate))
              (push keys ntoml--seen-keys)
              (setq ntoml--current-location keys)
              (let ((current-value (a-get-in ntoml--current keys)))
                (if current-value
                    (cond ((vectorp current-value))
                          ((listp current-value))
                          (t (ntoml-signal 'ntoml-table-redefine)))
                  (setq ntoml--current (a-assoc-in ntoml--current keys nil))))
              keys)
          (ntoml-signal 'ntoml-table-key-invalid))))))

(defun ntoml-read-inline-table ()
  (ntoml-preserve-point-on-fail
    (let (ret)
      (when (ntoml-skip-forward-regexp (rx "{"))
        (ntoml-read-whitespace)
        (setq ret (ntoml-read-inline-table-keyvals))
        (ntoml-read-whitespace)
        (when (ntoml-skip-forward-regexp (rx "}"))
          ;; We need a way to distinguish between an empty alist and
          ;; this function not finding anything.
          ;;
          ;; In the final output we can use nil to represent an empty
          ;; alist because false is represented by `:false', but here
          ;; we have to use a special value.
          ;;
          ;; This is then replaced with a nil again in
          ;; `ntoml-read-keyval'.
          ;;
          ;; A keyword is special enough as we do not decode anything
          ;; into keywords (other than one other value, `:false').
          (or ret :empty))))))

(defun ntoml-read-inline-table-keyvals ()
  (cl-loop when (ntoml-read-keyval :inline)
           collect it
           while (prog2
                     (ntoml-read-whitespace)
                     (ntoml-skip-forward-regexp (rx ","))
                   (ntoml-read-whitespace))))

(defun ntoml-read-array-table ()
  "Read an array table.

On success:

- store keys into `ntoml--current-location'
- enable `ntoml--reading-array-table'
- ensure keys exists in `ntoml--current'
- return keys

When we're not reading an array table, return nil.

When we're reading something invalid, signal an error."
  (ntoml-preserve-point-on-fail
    (let (keys)
      (when (ntoml-skip-forward-regexp (rx "[[") :once)
        (ntoml-read-whitespace)
        (setq keys (ntoml-read-key))
        (ntoml-read-whitespace)
        (if (ntoml-skip-forward-regexp (rx "]]") :once)
            (when keys
              (unless (listp keys)
                (setq keys (list keys)))
              (ntoml-array-table-flush)
              (setq ntoml--current-location keys
                    ntoml--reading-array-table t
                    ntoml--array-table-pending nil)
              (unless (a-get-in ntoml--current keys)
                (setq ntoml--current (a-assoc-in ntoml--current keys [])))
              keys)
          (ntoml-signal 'ntoml-table-key-invalid))))))

;; Inline Table
;; Array Table

(provide 'ntoml)

;;; ntoml.el ends here
