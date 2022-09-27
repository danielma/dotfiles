;;; dm-langs.el --- Misc langs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?_ "w"))))

(use-package rustic
  :disabled
  :config
  (setq rustic-format-on-save t
        rustic-format-display-method 'display-buffer))


(provide 'dm-langs)
;;; dm-langs.el ends here
