;;; dm-tabs --- My tabs configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:
(defun dm-tab-bar-name ()
  "Return the current buffer name padded or truncated to 20 columns."
  (let ((tab-name (buffer-name (window-buffer (minibuffer-selected-window)))))
    (concat " " (truncate-string-to-width tab-name 20 0 ?\s "…") " ")))

(use-package tab-bar
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-show t)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function 'dm-tab-bar-name)
  :config
  (tab-bar-mode))

(provide 'dm-tabs)

;;; dm-tabs.el ends here
