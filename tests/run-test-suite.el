;; -*- mode: lisp-interaction; lexical-binding: t; -*-

(defun t/run-cmd (&rest cmd)
  "Run CMD for its side effects."
  (apply #'call-process (car cmd) nil nil nil (cdr cmd)))

(url-copy-file "https://github.com/BurntSushi/toml-test/releases/download/v1.1.0/toml-test-v1.1.0-linux-amd64.gz" "toml-test.gz")
(t/run-cmd "7z" "x" "toml-test.gz")
(chmod "toml-test-v1.1.0-linux-amd64" #o755)
(rename-file "toml-test-v1.1.0-linux-amd64" "toml-test")
