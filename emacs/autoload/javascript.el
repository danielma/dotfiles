(defun my-javascript-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (back-to-indentation)
      (current-column)))
  )

(defun my-javascript-mode-setup ()
  (hs-minor-mode 1)
  (eslintd-fix-mode)
  (modify-syntax-entry ?` "\"" js-mode-syntax-table)
  (setq-local electric-indent-chars
              (append "<>" electric-indent-chars))
  (font-lock-add-keywords 'js-jsx-mode
                          '(("\\(?:</?\\([a-zA-Z0-9.]+\\)\\)" 1 'web-mode-html-tag-face)))
  ;; (setq-local outline-regexp " *\\(function\\|class\\|describe\\|it(\\)")
  ;; (setq-local outline-level 'my-javascript-outline-level)

  ;; (outline-minor-mode 1)
  (emmet-mode 1)
  (setq-local emmet-expand-jsx-className? t)
  (setq-local emmet-self-closing-tag-style " /")
  )

(define-abbrev-table 'javascript-mode-abbrev-table '(
                                                     ("aseq" "assert.equal")))

(sp-local-pair '(js-mode js-jsx-mode typescript-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair '(js-mode js-jsx-mode typescript-mode) "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(add-hook 'js-jsx-mode-hook #'smartparens-mode)
(add-hook 'js-jsx-mode-hook 'my-javascript-mode-setup)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-jsx-mode))
