;;; dm-completion.el --- Global bindings
;;; Commentary:

;;; Code:

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :bind (:map base-leader-map
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
  (ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
  (ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                           (swiper . ivy--regex-plus)
                           (t . ivy-fuz-regex-fuzzy)))
  (ivy-fuz-sort-limit ivy-sort-max-size)
  :config
  (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn)))

(use-package counsel
  :init
  (counsel-mode 1))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode)
  :custom
  (counsel-projectile-rg-initial-input '(ivy-thing-at-point))
  (counsel-projectile-switch-project-action 'my/projectile-switch-command)
  (counsel-rg-base-command "rg --with-filename --no-heading --line-number --color never --max-filesize 80K %s"))

(use-package ivy-posframe
  :init
  (ivy-posframe-mode 1)
  :custom
  (ivy-posframe-parameters '((left-fringe . 8)
                             (right-fringe . 8)))
  (ivy-posframe-style 'frame-top-center)
  (ivy-posframe-display-functions-alist
   '((swiper          . ivy-posframe-display-at-point)
     (complete-symbol . ivy-posframe-display-at-point)
     (t               . ivy-posframe-display))))

(use-package ivy-hydra)

(defalias 'my/m-x 'counsel-M-x)

(provide 'dm-completion)
;;; dm-completion.el ends here
