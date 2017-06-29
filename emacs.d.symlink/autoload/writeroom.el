(defun my/writeroom-setup ()
  (visual-line-mode t)
  (nlinum-mode 0)
  ;; (flyspell-mode)
  (if elscreen-display-tab
      (elscreen-toggle-display-tab))
  )

(evil-define-minor-mode-key
  'normal
  'writeroom-mode
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line)

(add-hook 'writeroom-mode-hook 'my/writeroom-setup)
