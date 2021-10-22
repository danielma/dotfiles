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
	 :map base-leader-map
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

(use-package flx-ido
  :disabled
  :init
  (ido-mode 1)
  ;; (ido-everywhere 1)
  (flx-ido-mode 1)
  :bind (:map ido-completion-map
	 ("C-k" . kill-whole-line)))

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ;; ivy-sort-matches-functions-alist '((counsel-M-x . nil)
        ;;                                    (counsel-yank-pop . nil)
        ;;                                    (t . ivy--shorter-matches-first))
        ;; ivy-re-builders-alist '(;; (projectile-completing-read . ivy--regex-fuzzy)
        ;;                         (counsel-rg . ivy--regex-plus)
        ;;                         (swiper . ivy--regex-plus)
        ;;                         (counsel-M-x . ivy--regex-plus)
        ;;                         ;; (counsel-projectile-find-file . ivy--regex-fuzzy)
        ;;                         (t . ivy--regex-plus))
        )
  :bind (:map base-leader-map
              ("bs" . ivy-switch-buffer)
              ("ho" . swiper-thing-at-point)
              ("hr" . ivy-resume)
              ("hk" . counsel-yank-pop)
              ("hi" . counsel-imenu)))

(use-package fuz
  :straight (:host github
             :repo "rustify-emacs/fuz.el"
             :branch "master"
             :files ("*"))
  :init
  (require 'fuz)
  (unless (require 'fuz-core nil t)
    (fuz-build-and-load-dymod)))

(use-package ivy-fuz
  :after fuz
  :if (require 'fuz-core nil 'noerror)
  :straight (:host github
             :repo "weiwee/ivy-fuz.el"
             :branch "master")
  :custom
  ;; (ivy-sort-functions-alist '((t . nil)))
  ;;                             ;; (counsel-minor . ivy-string<)
  ;;                             ;; (counsel-colors-web . ivy-string<)
  ;;                             ;; (counsel-unicode-char . ivy-string<)
  ;;                             ;; (counsel-register . ivy-string<)
  ;;                             ;; (counsel-mark-ring . ivy-string<)
  ;;                             ;; (counsel-file-register . ivy-string<)
  ;;                             ;; (counsel-describe-face . ivy-string<)
  ;;                             ;; (counsel-info-lookup-symbol . ivy-string<)
  ;;                             ;; (counsel-apropos . ivy-string<)
  ;;                             ;; (counsel-describe-symbol . ivy-string<)
  ;;                             ;; (read-file-name-internal . ivy-sort-file-function-default)
  ;;                             ;; (t . ivy-string<)))
  (ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
  (ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                           (swiper . ivy--regex-plus)
                           (t . ivy-fuz-regex-fuzzy)))
  (ivy-fuz-sort-limit 10000)
  :config
  (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

(use-package counsel
  :init
  (counsel-mode 1))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode)
  :config
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point)
        counsel-projectile-switch-project-action 'my/projectile-switch-command
        counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never --max-filesize 80K %s"))

(use-package ivy-posframe
  :init
  (ivy-posframe-mode 1)
  :config
  (setq ivy-posframe-parameters '((left-fringe . 8)
                                  (right-fringe . 8))
        ivy-posframe-style 'frame-top-center
        ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          (t               . ivy-posframe-display))))

(use-package ivy-hydra)

(use-package snails
  :disabled
  :after exec-path-from-shell
  :straight (:host github :repo "manateelazycat/snails"
                   :branch "master" :no-byte-compile t)
  :init
  (evil-set-initial-state 'snails-mode 'emacs)
  )

(use-package ivy-prescient
  :disabled
  :after
  ivy
  :init
  (ivy-prescient-mode)
  (prescient-persist-mode)
  :custom
  (prescient-filter-method '(literal regexp initialism fuzzy))
  )

;; (use-package amx)

(defalias 'my/m-x 'counsel-M-x)

(provide 'dm-completion)
