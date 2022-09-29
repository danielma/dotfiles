;;; dm-prog.el --- -*- lexical-binding: t -*-

(use-package apheleia
  :delight "\uf789"
  :config
  (push '(syntax-tree . ("bundle" "exec" "stree" "format")) apheleia-formatters)
  (push '(emacs-lisp-mode . lisp-indent) apheleia-mode-alist)
  (setf (alist-get 'ruby-mode apheleia-mode-alist)
        '(syntax-tree)))

(provide 'dm-prog)
