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

(use-package lua-mode
  :defer t)

(use-package swift-mode
  :hook
  (swift-mode . eglot))

(use-package fish-mode
  :defer t)

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(provide 'dm-langs)
;;; dm-langs.el ends here
