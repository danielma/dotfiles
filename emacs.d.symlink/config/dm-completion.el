;;; dm-completion.el --- Global bindings
;;; Commentary:

;;; Code:

(use-package vertico
  :init
  (vertico-mode))

(add-to-list 'load-path (straight--build-dir "vertico/extensions"))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode 1)
  :custom
  (vertico-posframe-border-width 1)
  (vertico-posframe-parameters '((left-fringe . 4) (right-fringe . 4)))
  (vertico-posframe-poshandler #'posframe-poshandler-frame-top-center))

(use-package embark
  :bind (:map minibuffer-mode-map ("C-." . embark-act)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package mini-frame
  :disabled
  :config
  (mini-frame-mode)
  :custom
  (mini-frame-color-shift-step 4)
  (mini-frame-show-parameters
   '((top . 0)
     (width . 0.7)
     (left . 0.5))))

(use-package posframe)

(defun mars/company-backend-with-yas (backends)
  "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
  (if (and (listp backends) (memq 'company-yasnippet backends))
      backends
    (append (if (consp backends)
                backends
              (list backends))
            '(:with company-yasnippet))))

(use-package company
  :disabled
  :after (delight yasnippet)
  :delight
  :custom
  (company-dabbrev-downcase nil) ; otherwise everything was lowercase
  :bind (:map global-map
              ("C-'" . company-complete)
              :map company-active-map
              ("<tab>" . company-complete-selection))
  :config
  (global-company-mode)
  ;; add yasnippet to all backends
  (setq company-backends (mapcar #'mars/company-backend-with-yas company-backends)))


(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  :bind
  (:map global-map
        ("C-'" . completion-at-point)
        :map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; ;; Part of corfu
;; (use-package corfu-popupinfo
;;   :after corfu
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :custom
;;   (corfu-popupinfo-delay '(0.25 . 0.1))
;;   (corfu-popupinfo-hide nil)
;;   :config
;;   (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))



(if t
    t

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
    (ivy-sort-matches-functions-alist '((swiper . ivy-sort-identity) (t . ivy-fuz-sort-fn)))
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

  (defun ivy-sort-identity (pattern cands)
    "Identity sort that always returns the first argument."
    cands)
  )

(provide 'dm-completion)
;;; dm-completion.el ends here
