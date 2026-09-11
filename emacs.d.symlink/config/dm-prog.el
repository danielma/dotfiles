;;; dm-prog.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(with-eval-after-load 'prog-mode
  (add-hook 'prog-mode-hook 'flymake-mode))

;; Apheleia

(defvar-local apheleia--syntax-tree-stree-location nil)
(defvar-local apheleia--syntax-tree-single-quotes nil)

(defun apheleia--syntax-tree-stree-location ()
  "Location of a valid .stree file."
  (if (eq apheleia--syntax-tree-stree-location 'nope)
      nil
    (or apheleia--syntax-tree-stree-location
        (let ((root (locate-dominating-file (or (buffer-file-name) default-directory) ".streerc")))
          (setq apheleia--syntax-tree-stree-location (or root 'nope))
          (apheleia--syntax-tree-stree-location)))))

(defun --with-project-default-directory (orig-fun &rest args)
  (if (project-current)
      (with-project-default-directory (apply orig-fun args))
    (apply orig-fun args)))

(use-package rustic
  :defer t
  :custom
  (rustic-lsp-client 'eglot))

(defvar my/eglot-format-modes '(rust-mode rust-ts-mode)
  "Modes where eglot should handle formatting instead of apheleia.")

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (when (and (eglot-managed-p)
                       (apply #'derived-mode-p my/eglot-format-modes))
              (apheleia-mode -1)
              (add-hook 'before-save-hook #'eglot-format-buffer -10 t))))

(use-package apheleia
  :commands apheleia-global-mode
  :delight "􀋺"
  :init
  (apheleia-global-mode)
  :config
  (advice-add 'apheleia--make-process :around '--with-project-default-directory)
  (push '(syntax-tree . ((when (apheleia--syntax-tree-stree-location) (list "bundle" "exec"))
                         "stree" "format"))
        apheleia-formatters)
  ;; (push '(syntax-tree . ("stree" "format" "--print-width=100" (when apheleia--syntax-tree-single-quotes "--plugins=plugin/single_quotes"))) apheleia-formatters)
  (push '(rubyfmt . ("rubyfmt" "--")) apheleia-formatters)
  (push '(eslint . (npx "eslint" "--fix-dry-run" "--stdin" "--stdin-filename" filepath "-f" "/Users/danielma/.dotfiles/javascript/eslint-output-formatter.js" "--max-warnings" "10000")) apheleia-formatters)
  (push '(swift-format . ("swift-format")) apheleia-formatters)

  (push '(emacs-lisp-mode . lisp-indent) apheleia-mode-alist)
  (push '(swift-mode . swift-format) apheleia-mode-alist)
  ;; (push '(tsx-ts-mode . eslint) apheleia-mode-alist)
  (setf (alist-get 'ruby-base-mode apheleia-mode-alist)
        '(syntax-tree))
  (setf (alist-get 'ruby-ts-mode apheleia-mode-alist)
        '(syntax-tree))
  (setf (alist-get 'js-mode apheleia-mode-alist)
        '(eslint)))

(setopt treesit-enabled-modes t
        treesit-auto-install-grammar 'ask)

(use-package treesit-fold
  :delight
  :config
  (global-treesit-fold-mode))

(use-package smartparens
  :delight
  :init
  (require 'smartparens-config)
  :custom
  (sp-ignore-modes-list '(minibuffer-mode minibuffer-inactive-mode web-mode))
  ;; (sp-base-key-bindings 'sp)
  :config
  (smartparens-global-mode)
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode rjsx-mode ruby-base-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode rjsx-mode ruby-base-mode) "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  )

(defun sp-wrap-interactive (pair)
  "Inreactive function to wrap with PAIR."
  (interactive "cPair:")
  (sp-wrap-with-pair (char-to-string pair)))

(use-package iedit
  :commands iedit-mode)

(use-package transient)

(provide 'dm-prog)
