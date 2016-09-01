(defun my/writeroom-setup ()
  (visual-line-mode t)
  (linum-mode 0)
  (if elscreen-display-tab
      (elscreen-toggle-display-tab))
  )

(add-hook 'writeroom-mode-hook 'my/writeroom-setup)
