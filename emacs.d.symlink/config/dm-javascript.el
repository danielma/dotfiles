(use-package smartparens
  :config
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode rjsx-mode ruby-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode rjsx-mode ruby-mode) "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  )

(use-package eslintd-fix)

(use-package rjsx-mode
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  :init
  (add-hook 'rjsx-mode-hook 'eslintd-fix-mode)
  (add-hook 'rjsx-mode-hook 'emmet-mode)
  (add-hook 'rjsx-mode-hook 'smartparens-mode)
  (add-hook 'rjsx-mode-hook 'dm-guard-mode)
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook (lambda ()
			      (setq-local emmet-expand-jsx-className? t)
			      (setq-local emmet-self-closing-tag-style " /")))
  (evil-define-key 'insert rjsx-mode-map
    (kbd "C-d") 'rjsx-delete-creates-full-tag)
  :bind (:map rjsx-mode-map
	      ))

(use-package tide
  :after flycheck)

(defun setup-tide-mode ()
  "Setup for tide mode."
  (eslintd-fix-mode +1)
  (tide-setup)
  (dm-guard-mode)
  (eldoc-mode +1)
  (smartparens-mode +1)
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
  (tide-hl-identifier-mode +1))
  
(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :after tide
  :init
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  :custom
  (typescript-indent-level 2)
  )


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
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-jsx-mode))

(provide 'dm-javascript)
