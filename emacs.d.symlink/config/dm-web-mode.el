;;; dm-web-mode.el --- Rails web templates -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun dm-web-mode-setup ()
  "Configure indentation for the current Web Mode buffer."
  (setq-local electric-indent-chars
              (delete-dups (append '(?{ ?} ?\;) electric-indent-chars)))
  (setq-local emmet-indentation tab-width
              web-mode-attr-indent-offset tab-width
              web-mode-code-indent-offset tab-width
              web-mode-css-indent-offset tab-width
              web-mode-markup-indent-offset tab-width
              web-mode-sql-indent-offset tab-width))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\(?:+modal\\)?\\.erb\\'" . web-mode)
         ("\\.js\\.erb\\'" . web-mode))
  :hook
  (web-mode . dm-web-mode-setup)
  :config
  (define-abbrev web-mode-abbrev-table "tt" "<%")
  (define-abbrev web-mode-abbrev-table "tp" "<%="))

(use-package emmet-mode
  :ensure t
  :hook
  (web-mode . emmet-mode))

(provide 'dm-web-mode)
;;; dm-web-mode.el ends here
