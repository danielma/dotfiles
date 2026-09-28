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

(ert-deftest dm-apply-terminal-default-colors-uses-doom-palette ()
  (let (background foreground)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _display) nil))
              ((symbol-function 'doom-color)
               (lambda (name &optional _type)
                 (pcase name
                   ('bg "#fafafa")
                   ('fg "#2a2a2a"))))
              ((symbol-function 'set-background-color)
               (lambda (color) (setq background color)))
              ((symbol-function 'set-foreground-color)
               (lambda (color) (setq foreground color))))
      (dm-apply-terminal-default-colors))
    (should (equal background "#fafafa"))
    (should (equal foreground "#2a2a2a"))))

(ert-deftest dm-apply-terminal-default-colors-skips-graphical-frames ()
  (let (called)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _display) t))
              ((symbol-function 'set-background-color)
               (lambda (_color) (setq called t)))
              ((symbol-function 'set-foreground-color)
               (lambda (_color) (setq called t))))
      (dm-apply-terminal-default-colors))
    (should-not called)))

(ert-deftest dm-terminal-default-colors-run-after-frame-setup ()
  (should (memq #'dm-apply-terminal-default-colors window-setup-hook)))

;;; dm-colors-test.el ends here
