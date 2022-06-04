;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'ntoml)
(require 'buttercup)

(defun test-buf (func string)
  "Insert STRING in a temporary buffer then run FUNC at the start of it."
  (with-current-buffer (get-buffer-create "*test-ntoml*")
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (funcall func)))

(describe "ntoml-read-boolean"
  (it "ignores strings"
    (expect (test-buf #'ntoml-read-boolean "nottrue")
            :to-be nil)
    (expect (test-buf #'ntoml-read-boolean "\"true\"")
            :to-be nil))
  (it "ignores interers"
    (expect (test-buf #'ntoml-read-boolean "true")
            :to-be t)
    (expect (test-buf #'ntoml-read-boolean "false")
            :to-be :false)))

(describe "ntoml-read-float"
  (it "ignores interers"
    (expect (test-buf #'ntoml-read-float "-300")
            :to-be nil)
    (expect (test-buf #'ntoml-read-float "+300")
            :to-be nil)
    (expect (test-buf #'ntoml-read-float "300")
            :to-be nil))
  (it "parses float"
    (expect (test-buf #'ntoml-read-float "300.3e+32")
            :to-equal 3.003e+34)
    (expect (test-buf #'ntoml-read-float "-300e+20")
            :to-equal -3e+22)
    (expect (test-buf #'ntoml-read-float "-3e+22")
            :to-equal -3e+22))
  (it "parses inf"
    (expect (test-buf #'ntoml-read-float "inf")
            :to-equal 1.0e+INF)
    (expect (test-buf #'ntoml-read-float "+inf")
            :to-equal 1.0e+INF)
    (expect (test-buf #'ntoml-read-float "-inf")
            :to-equal -1.0e+INF))
  (it "parses nan"
    (expect (test-buf #'ntoml-read-float "nan")
            :to-equal 1.0e+NaN)
    (expect (test-buf #'ntoml-read-float "+nan")
            :to-equal 1.0e+NaN)
    (expect (test-buf #'ntoml-read-float "-nan")
            :to-equal -1.0e+NaN)))
