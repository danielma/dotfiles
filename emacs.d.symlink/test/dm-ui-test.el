;;; dm-ui-test.el --- Tests for dm-ui -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defmacro use-package (&rest _args)
  "Ignore package configuration while loading dm-ui for unit tests."
  nil)

(load (expand-file-name
       "../config/dm-ui.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest dm-hide-menu-bar-in-terminal-frame-hides-menu-bar ()
  (let (parameter)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) nil))
              ((symbol-function 'set-frame-parameter)
               (lambda (_frame name value)
                 (setq parameter (cons name value)))))
      (dm-hide-menu-bar-in-terminal-frame 'terminal-frame))
    (should (equal parameter '(menu-bar-lines . 0)))))

(ert-deftest dm-hide-menu-bar-in-terminal-frame-preserves-graphical-menu-bar ()
  (let (called)
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (&rest _args) (setq called t))))
      (dm-hide-menu-bar-in-terminal-frame 'graphical-frame))
    (should-not called)))

(ert-deftest dm-hide-menu-bar-in-terminal-frame-runs-for-new-frames ()
  (should (memq #'dm-hide-menu-bar-in-terminal-frame
                after-make-frame-functions)))

;;; dm-ui-test.el ends here
