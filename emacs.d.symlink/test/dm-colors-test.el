;;; dm-colors-test.el --- Tests for dm-colors -*- lexical-binding: t; -*-

(require 'ert)

;; The parser is independent of the packages configured by `dm-colors'.
(defmacro use-package (&rest _args)
  "Ignore package configuration while loading dm-colors for unit tests."
  nil)

(load (expand-file-name
       "../config/dm-colors.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest dm-osc-11-response-appearance-detects-dark-background ()
  (should (eq 'dark
              (dm-osc-11-response-appearance
               "\e]11;rgb:0000/0000/0000\e\\"))))

(ert-deftest dm-osc-11-response-appearance-detects-light-background ()
  (should (eq 'light
              (dm-osc-11-response-appearance
               "\e]11;rgb:ffff/ffff/ffff\a"))))

(ert-deftest dm-osc-11-response-appearance-rejects-malformed-response ()
  (should-not (dm-osc-11-response-appearance "not an OSC response")))

;;; dm-colors-test.el ends here
