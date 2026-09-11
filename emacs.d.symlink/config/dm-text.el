;;; dm-text.el --- Text abbreviations -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package abbrev
  :ensure nil
  :custom
  (save-abbrevs nil)
  :hook
  ((prog-mode text-mode) . abbrev-mode)
  :config
  (define-abbrev global-abbrev-table "orgn" "organization")
  (define-abbrev global-abbrev-table "Scoto" "ScopedToOrganization"))

(provide 'dm-text)
;;; dm-text.el ends here
