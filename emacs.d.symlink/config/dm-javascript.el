(use-package smartparens
  :config
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode) "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  )
(use-package eslintd-fix)

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun my-javascript-mode-setup ()
  (eslintd-fix-mode)
  (setq-local electric-indent-chars
              (append "<>" electric-indent-chars))
  (font-lock-add-keywords 'js-jsx-mode
                          '(("\\(?:</?\\([a-zA-Z0-9.]+\\)\\)" 1 'web-mode-html-tag-face)))
  (emmet-mode 1)
  (setq-local emmet-expand-jsx-className? t)
  (setq-local emmet-self-closing-tag-style " /")
  )

(define-abbrev-table 'javascript-mode-abbrev-table '(
                                                     ("aseq" "assert.equal")))
(define-abbrev-table 'js-jsx-mode-abbrev-table '(
                                                     ("aseq" "assert.equal")))

(add-hook 'js-jsx-mode-hook #'smartparens-mode)
(add-hook 'js-jsx-mode-hook 'my-javascript-mode-setup)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-jsx-mode))

(provide 'dm-javascript)
