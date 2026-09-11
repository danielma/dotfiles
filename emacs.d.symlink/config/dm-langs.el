;;; dm-langs.el --- Misc langs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun my/yaml-mode-setup ()
  (require 'whitespace)
  (add-to-list 'whitespace-style 'spaces)
  (modify-syntax-entry ?_ "w"))

(use-package yaml-ts-mode
  :ensure nil
  :hook
  (yaml-ts-mode . my/yaml-mode-setup))

(use-package lua-mode
  :defer t)

(use-package swift-mode
  :hook
  (swift-mode . eglot-ensure))

(use-package fish-mode
  :defer t)

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(provide 'dm-langs)
;;; dm-langs.el ends here
