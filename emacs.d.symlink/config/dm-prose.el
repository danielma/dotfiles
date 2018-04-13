(defun my/markdown-mode-setup ()
  (visual-line-mode nil)
  (whitespace-mode 0)
  (setq-local word-wrap t))

(use-package markdown-mode
  :config
  (add-hook 'markdown-mode-hook 'my/markdown-mode-setup))

(defun my/writeroom-mode-setup ()
  )

(use-package writeroom-mode
  :config
  (add-hook 'writeroom-mode-hook 'my/writeroom-mode-setup)
  (setq writeroom-extra-line-spacing 0.3
        writeroom-fullscreen-effect nil
        writeroom-global-effects (quote ())
        writeroom-maximize-window nil
        writeroom-width 100
	writeroom-restore-window-config nil))

(use-package simplenote2
  :config
  (setq simplenote2-markdown-notes-mode 'markdown-mode
        simplenote2-notes-mode 'markdown-mode
        simplenote2-show-note-file-name nil
        simplenote2-list-sort-key '("Modified" . nil))
  (let ((info (nth 0 (auth-source-search :host "app.simplenote.com" :require '(:user :secret)))))
    (if info
        (let ((secret (plist-get info :secret))
              (user (plist-get info :user)))
          (if (functionp secret)
              (setq simplenote2-email user
                    simplenote2-password (funcall secret))
            (message "can't authenticate simplenote2")))))
  (add-hook 'simplenote2-note-mode-hook 'markdown-mode)
  :commands simplenote2-setup)

(provide 'dm-prose)
