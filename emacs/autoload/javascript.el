(add-hook 'javascript-mode-hook 'my-javascript-mode-setup)

(defun my-javascript-outline-level ()
  (let (buffer-invisibility-spec)
    (save-excursion
      (back-to-indentation)
      (current-column)))
  )

(defun my-javascript-mode-setup ()
  (hs-minor-mode 1)
  (modify-syntax-entry ?` "\"" js-mode-syntax-table)
  ;; (setq-local outline-regexp " *\\(function\\|class\\|describe\\|it(\\)")
  ;; (setq-local outline-level 'my-javascript-outline-level)

  ;; (outline-minor-mode 1)
  )

(sp-local-pair '(js-mode typescript-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair '(js-mode typescript-mode) "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(add-hook 'js-mode-hook #'smartparens-mode)

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
