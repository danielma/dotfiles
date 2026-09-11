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
  (tab-bar-show 1)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function 'dm-tab-bar-name)
  :config
  (tab-bar-mode))

(use-package transient
  :ensure nil
  :config
  (transient-define-prefix tab-bar-transient ()
    "Tab-bar menu"
    [["Creation"
      ("t" "new tab" tab-bar-new-tab)]
     ["Movement"
      ("RET" "switch tab" tab-switch)
      ("n" "next tab" tab-next :transient t)
      ("p" "previous tab" tab-previous :transient t)
      ("h" "move left" tab-bar-move-tab-backward :transient t)
      ("l" "move right" tab-bar-move-tab :transient t)]]
    [["Management"
      ("r" "rename tab" tab-rename)
      ("x" "close tab" tab-close)]]
    [[""
      ("q" "Quit" transient-quit-one)]])
  :bind (:map global-map
              ("C-c C-t" . tab-bar-transient)))

(provide 'dm-tabs)

;;; dm-tabs.el ends here
