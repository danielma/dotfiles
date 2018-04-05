(defun my/markdown-mode-setup ()
  (visual-line-mode nil)
  (whitespace-mode 0)
  (setq-local word-wrap t))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'my/markdown-mode-setup))

(defun my/writeroom-mode-setup ()
  (nlinum-mode 0))

(use-package writeroom-mode
  :config
  (add-hook 'writeroom-mode-hook 'my/writeroom-mode-setup)
  (setq writeroom-extra-line-spacing 0.3
        writeroom-fullscreen-effect nil
        writeroom-global-effects (quote ())
        writeroom-maximize-window nil
        writeroom-width 100
	writeroom-restore-window-config nil))

(provide 'dm-prose)
