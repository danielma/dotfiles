;;; dm-langs.el --- Misc langs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun my/yaml-mode-setup ()
  (add-to-list 'whitespace-style 'spaces))

(use-package yaml-mode
  :hook
  (yaml-mode . my/yaml-mode-setup)
  :config
  (add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?_ "w"))))

(use-package rustic
  :disabled
  :config
  (setq rustic-format-on-save t
        rustic-format-display-method 'display-buffer))

(use-package swift-mode
  :hook
  (swift-mode . eglot-ensure))

(use-package fish-mode)

(provide 'dm-langs)
;;; dm-langs.el ends here
