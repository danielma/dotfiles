(defun my-web-mode-setup ()
  (setq-local electric-indent-chars
              (append "{};" electric-indent-chars))
  (if (member web-mode-engine '("php" "erb"))
      (modify-syntax-entry ?_ "w"))
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")))

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
  (setq web-mode-engines-alist
	'(("php" . "\\.module\\'")))
  )

(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode))

(provide 'dm-web-mode)
