(use-package projectile
  :init
  (projectile-global-mode)

  :config
  (evil-leader/set-key "p" projectile-command-map)

  (use-package helm-projectile
    :init
    (helm-projectile-on)
    :bind (:map projectile-command-map
		("a" . helm-projectile-ag)))
  (use-package helm-ag)

  :bind (:map projectile-command-map
	("T" . projectile-find-implementation-or-test-other-window)))

(provide 'dm-projectile)
