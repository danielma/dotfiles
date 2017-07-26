(use-package projectile
  :init
  (projectile-mode)

  :config
  (evil-leader/set-key "p" projectile-command-map)

  (use-package helm-projectile
    :init
    (helm-projectile-on)
    :bind (:map projectile-command-map
		("a" . helm-projectile-ag)))
  (use-package helm-ag
    :init
    (setq helm-ag-fuzzy-match t)
    (setq helm-ag-insert-at-point (quote symbol))
    )

  (setq projectile-generic-command "ag -g \"\"")
  (setq projectile-switch-project-action '(projectile-dired))

  :bind (:map projectile-command-map
	("T" . projectile-find-implementation-or-test-other-window)))

(provide 'dm-projectile)
