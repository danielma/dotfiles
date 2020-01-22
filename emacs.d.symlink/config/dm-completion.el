(defalias 'my/m-x 'helm-M-x)

(use-package helm
  :disabled
  :config
  (setq helm-completion-in-region-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-mode-fuzzy-match t
        helm-imenu-fuzzy-match t
	helm-follow-mode-persistent t)
  (helm-mode)
  :bind (
	 ("C-." . my/m-x)
	 ;; iterm c-.
	 :map base-leader-map
	 ("<SPC>" . my/m-x)
	 ("ho" . helm-occur)
	 ("hr" . helm-resume)
	 ("hb" . helm-bookmarks)
	 ("hm" . helm-all-mark-rings)
	 ("hk" . helm-show-kill-ring)
         ("hg" . helm-register)
         ("hi" . helm-imenu)))

(use-package helm-ag
  :disabled
  :after helm
  :init
  (setq helm-ag-fuzzy-match t)
  (setq helm-ag-insert-at-point (quote symbol))
  )

(use-package helm-projectile
  :disabled
  :after projectile
  :init
  (helm-projectile-on)
  :bind (:map projectile-command-map
              ("a" . helm-projectile-ag)))

(use-package helm-posframe
  :disabled
  :after helm
  :init
  (helm-posframe-disable)
  :config
  (setq helm-posframe-poshandler 'posframe-poshandler-frame-top-center
        helm-posframe-width 920
        helm-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10))))

(use-package fuz
  :disabled
  :straight (:host github
             :repo "rustify-emacs/fuz.el"
             :branch "master"
             :files ("*"))
  :init
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package snails
  :after exec-path-from-shell
  :straight (:host github :repo "manateelazycat/snails"
                   :branch "master" :no-byte-compile t)
  :init
  (evil-set-initial-state 'snails-mode 'emacs)
  )

(provide 'dm-completion)
