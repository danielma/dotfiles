;;; dm-ruby.el --- Ruby support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ruby-mode
  :ensure nil
  :hook
  (ruby-base-mode . dm-guard-mode))

(provide 'dm-ruby)
;;; dm-ruby.el ends here
