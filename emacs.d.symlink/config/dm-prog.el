;;; dm-prog.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Apheleia

(defvar-local apheleia--syntax-tree-stree-location nil)

(defun apheleia--syntax-tree-stree-location ()
  "Location of a valid .stree file."
  (if (eq apheleia--syntax-tree-stree-location 'nope)
      nil
    (or apheleia--syntax-tree-stree-location
        (let ((root (locate-dominating-file (or (buffer-file-name) default-directory) ".streerc")))
          (setq apheleia--syntax-tree-stree-location (or root 'nope))))
    (apheleia--syntax-tree-stree-location)))

(use-package apheleia
  :delight "\uf789"
  :config
  (apheleia-global-mode)
  ;; (push '(syntax-tree . ((when (apheleia--syntax-tree-stree-location) (list "bundle" "exec"))
  ;;                        "stree" "format"))
  ;;       apheleia-formatters)
  (push '(syntax-tree . ("stree" "format" "--print-width=100" "--plugins=plugin/single_quotes")) apheleia-formatters)
  (push '(rubyfmt . ("rubyfmt" "--")) apheleia-formatters)
  (push '(emacs-lisp-mode . lisp-indent) apheleia-mode-alist)
  (setf (alist-get 'ruby-mode apheleia-mode-alist)
        '(syntax-tree)))

;;; Tree Sitter

(use-package tree-sitter)
(use-package tree-sitter-langs
  :after tree-sitter)

(provide 'dm-prog)
