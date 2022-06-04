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

(describe "ntoml-read-string"
  (it "ignores  values"
    (expect (test-buf #'ntoml-read-string "true")
            :to-be nil)
    (expect (test-buf #'ntoml-read-string "not quoted")
            :to-be nil))
  (it "parses strings"
    (expect (test-buf #'ntoml-read-string "\"my string\"")
            :to-equal
            "my string")
    (expect (test-buf #'ntoml-read-string "'my literal string'")
            :to-equal
            "my literal string"))
  (xit "escapes sequences strings"
    (expect (test-buf #'ntoml-read-string "\"my string\\nwith a newline\"")
            :to-equal
            "my string\nwith a newline")))

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
    (expect (test-buf #'ntoml-read-float "3.5")
            :to-equal 3.5)
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

(describe "ntoml-read-integer"
  ;; It doesn't need to ignore floats as we only run it after knowing
  ;; the value is not a float.
  (it "barfs on leading zeros"
    (expect (test-buf #'ntoml-read-integer "01")
            :to-throw
            'ntoml-integer-leading-zero))
  (it "parses an integer"
    (expect (test-buf #'ntoml-read-integer "300")
            :to-equal 300)
    (expect (test-buf #'ntoml-read-integer "+300")
            :to-equal 300)
    (expect (test-buf #'ntoml-read-integer "-300")
            :to-equal -300))
  (it "parses hex"
    (expect (test-buf #'ntoml-read-integer "0xAB")
            :to-equal #xAB))
  (it "parses octal"
    (expect (test-buf #'ntoml-read-integer "0o7777")
            :to-equal #o7777))
  (it "parses binary"
    (expect (test-buf #'ntoml-read-integer "0b10011001")
            :to-equal #b10011001))
  (it "parses separators"
    (expect (test-buf #'ntoml-read-integer "300_000_000")
            :to-equal 300000000)
    (expect (test-buf #'ntoml-read-integer "0b11111111_00000000")
            :to-equal #b1111111100000000)
    (expect (test-buf #'ntoml-read-integer "0xABC_DE0")
            :to-equal #xABCDE0)
    (expect (test-buf #'ntoml-read-integer "0o567_654")
            :to-equal #o567654)))

(describe "ntoml-read-array"
  (it "parses an array"
    (expect (test-buf #'ntoml-read-array "[1, 2, 3, true]")
            :to-equal '(1 2 3 t))
    (expect (test-buf #'ntoml-read-array "[\"Hello\", 3.5]")
            :to-equal '("Hello" 3.5))
    (expect (test-buf #'ntoml-read-array "[]")
            :to-equal nil)))
