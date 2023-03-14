(defun my-web-mode-setup ()
  (setq-local electric-indent-chars
              (append "{};" electric-indent-chars))
  ;; (if (member web-mode-engine '("php" "erb"))
  ;; (modify-syntax-entry ?_ "w"))
  )

(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\(\+modal\\)?\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  :hook
  (web-mode . my-web-mode-setup)
  :config
  (define-abbrev-table 'web-mode-abbrev-table '(
						                                    ("tt" "<%")
						                                    ("tp" "<%=")))
  (dolist (width '(web-mode-attr-indent-offset web-mode-code-indent-offset web-mode-css-indent-offset web-mode-markup-indent-offset web-mode-sql-indent-offset))
    (set width tab-width)))

(use-package emmet-mode
  :hook
  (web-mode . emmet-mode)
  :config
  (defvaralias 'emmet-indentation 'tab-width))

(provide 'dm-web-mode)
