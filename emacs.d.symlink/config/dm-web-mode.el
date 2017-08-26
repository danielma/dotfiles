(defun my-web-mode-setup ()
  (setq-local electric-indent-chars
              (append "{};" electric-indent-chars))
  (if (equal web-mode-engine "php")
      (my/web-mode-php-setup))
  (if (member web-mode-engine '("php" "erb"))
      (modify-syntax-entry ?_ "w"))
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")))

(defun my/web-mode-php-setup ()
  "Web mode setup for php."
  )

(defun my/web-mode-control-colon ()
  (interactive)
  (end-of-line)
  (insert ";")
  (newline-and-indent))

(use-package web-mode
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\(\+modal\\)?\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.module\\'" . web-mode))
  (add-hook 'web-mode-hook 'my-web-mode-setup)
  (define-abbrev-table 'web-mode-abbrev-table '(
						("nd" "<% end %>")
						("tt" "<%")
						("tp" "<%=")))
  :config
  (dolist (width '(web-mode-attr-indent-offset web-mode-code-indent-offset web-mode-css-indent-offset web-mode-markup-indent-offset web-mode-sql-indent-offset))
    (set width 2))
  (setq web-mode-engines-alist
	'(("php" . "\\.module\\'")))
  :bind (:map web-mode-map
	      ("C-:" . my/web-mode-control-colon))
  )

(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  :config
  (setq emmet-indentation 2))

(provide 'dm-web-mode)
