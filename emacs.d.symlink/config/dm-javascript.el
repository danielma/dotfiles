;; -*- lexical-binding: t -*-

(use-package js
  :config
  (defvaralias 'js-indent-level 'tab-width))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens
  :delight ""
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode)
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode rjsx-mode ruby-base-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair '(js-mode js-jsx-mode typescript-mode rjsx-mode ruby-base-mode) "(" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
  )

(use-package typescript-ts-mode
  :hook
  (typescript-ts-mode . eglot-ensure))

(if t
    t


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
			                          (setq-local emmet-self-closing-tag-style " /")
                                (modify-syntax-entry ?_ "w")
                                ))
    (evil-define-key 'insert rjsx-mode-map
      (kbd "C-d") 'rjsx-delete-creates-full-tag)
    :bind (:map rjsx-mode-map
	              ))

  (use-package tide
    :disabled
    :after flycheck)

  (use-package eldoc-box)

  (defun setup-tide-mode ()
    "Setup for tide mode."
    (eslintd-fix-mode +1)
    (tide-setup)
    (dm-guard-mode)
    (eldoc-box-hover-at-point-mode +1)
    (emmet-mode +1)
    (modify-syntax-entry ?_ "w")
    (setq-local emmet-expand-jsx-className? t)
    (setq-local emmet-self-closing-tag-style " /")
    (smartparens-mode +1)
    (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    (tide-hl-identifier-mode +1))
  
  (use-package typescript-mode
    :mode "\\.tsx?\\'"
    ;; :after tide
    :init
    ;; (add-hook 'typescript-mode-hook #'setup-tide-mode)
    (add-hook 'typescript-mode-hook #'lsp)
    ;; (add-hook 'typescript-mode-hook #'prettier-js-mode)
    (add-hook 'typescript-mode-hook #'smartparens-mode)
    ;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)
    :custom
    (typescript-indent-level 2)
    :config
    (font-lock-add-keywords 'typescript-mode
                            '(("\\(?: </?\\([a-zA-Z0-9.]+\\)\\)" 1 'web-mode-html-tag-face)))
    )


  ;; (defun my-javascript-mode-setup ()
  ;;   (eslintd-fix-mode)
  ;;   (setq-local electric-indent-chars
  ;;               (append "<>" electric-indent-chars))
  ;;   (font-lock-add-keywords 'js-jsx-mode
  ;;                           '(("\\(?:</?\\([a-zA-Z0-9.]+\\)\\)" 1 'web-mode-html-tag-face)))
  ;;   (emmet-mode 1)
  ;;   (setq-local emmet-expand-jsx-className? t)
  ;;   (setq-local emmet-self-closing-tag-style " /")
  ;;   )
  ;; 
  ;; (define-abbrev-table 'javascript-mode-abbrev-table '(
  ;;                                                      ("aseq" "assert.equal")))
  ;; (define-abbrev-table 'js-jsx-mode-abbrev-table '(
  ;;                                                      ("aseq" "assert.equal")))
  ;; 
  ;; (add-hook 'js-jsx-mode-hook #'smartparens-mode)
  ;; (add-hook 'js-jsx-mode-hook 'my-javascript-mode-setup)
  ;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-jsx-mode))
  )

(provide 'dm-javascript)
