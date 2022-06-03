;;; ntoml.el --- Another TOML encoder / decoder -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
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

(defun ntoml-encode (value)
  "Encode VALUE as TOML."
  (json-serialize value))

(defun ntoml-decode-buffer ()
  "Decode current buffer contents as TOML and return the Elisp value."
  (save-excursion
    (goto-char (point-min))
    (json-parse-buffer)))

(defun ntoml-decode (toml-string)
  "Decode TOML-STRING into Elisp value."
  (with-temp-buffer
    (insert toml-string)
    (ntoml-decode-buffer)))

(provide 'ntoml)

;;; ntoml.el ends here
