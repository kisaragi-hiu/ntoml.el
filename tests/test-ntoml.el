;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(require 'pkgname)
(require 'buttercup)

(describe "hello"
  (it "says hello"
    (expect (pkgname-hello-world)
            :to-equal
            "Hello world!")))
