;;; .emacs --- take care of business
;;; Code:
;;; Commentary:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq gc-cons-threshold 20000000)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      make-backup-files nil
      indent-tabs-mode nil
      ns-use-native-fullscreen nil
      ad-redefinition-action 'accept
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      create-lockfiles nil
      frame-resize-pixelwise t)

;; (setq default-frame-alist '((undecorated . t)))
;; (setq default-frame-alist '())

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(fset 'evil-visual-update-x-selection 'ignore)

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t
      straight-use-package-by-default t)

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'text-tools)
(require 'general-funcs)
(require 'global-map)

;; set all widths to 2
;; (dolist (width '(evil-shift-width))
;;         (set width 2))

(use-package dm-bindings :straight nil)

(use-package helm
  :config
  (setq helm-completion-in-region-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-mode-fuzzy-match t
        helm-imenu-fuzzy-match t
	helm-follow-mode-persistent t)
  (helm-mode)
  :bind (
	 ("C-." . helm-M-x)
	 ;; iterm c-.
	 ("<f6>" . helm-M-x)
	 :map base-leader-map
	 ("<SPC>" . helm-M-x)
	 ("ho" . helm-occur)
	 ("hr" . helm-resume)
	 ("hb" . helm-bookmarks)
	 ("hm" . helm-all-mark-rings)
	 ("hk" . helm-show-kill-ring)
         ("hg" . helm-register)
         ("hi" . helm-imenu)))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-language-file-exts
        (print (add-to-list 'dumb-jump-language-file-exts
                     '(:language "php" :ext "module" :agtype "php"))))
  :bind (:map base-leader-map
	 ("sa" . dumb-jump-go)
	 ("sA" . dumb-jump-go-other-window)
	 ("sp" . dumb-jump-go-prompt)
	 ("sl" . dumb-jump-quick-look)))

(use-package buffer-move
  :commands (buf-move buf-move-right buf-move-left buf-move-up buf-move-down)
  )

(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  )

(use-package zoom-frm
  :config
  (defun my/zoom-frm-keep-frame-size (orig-fun &rest args)
    ;; (frame-pixel-height)
    ;; (set-frame-size
    ;; (assq 'pix-height (frame-parameters))
    ;; (frame-parameters)
    (let (;; (corrective-height (frcmds-available-screen-pixel-height) (frcmds-effective-screen-pixel-bounds)
          (fullscreen (frame-parameter nil 'fullscreen))
          (orig-height (frame-height))
          (orig-width (frame-width))
          (orig-pixel-height (frame-pixel-height))
          (orig-pixel-width (frame-pixel-width))
          (res (apply orig-fun args))
          (new-font-size (/ (face-attribute 'default :height) 10)))
      ;; (message "H:%S W:%S" orig-pixel-height orig-pixel-width)
      (if (memq fullscreen '(fullscreen fullboth))
          (message "hi")
          ;; (set-frame-size (selected-frame) orig-width orig-height)
          (set-frame-size
           (selected-frame)
           (- orig-pixel-width (* new-font-size 2))
           (- orig-pixel-height (/ new-font-size 4))
           'PIXELWISE))
      res))
  (setq frame-inhibit-implied-resize nil)
  (face-spec-recalc 'default (selected-frame))
  (advice-add 'zoom-frm-in :around #'my/zoom-frm-keep-frame-size)
  (advice-add 'zoom-frm-out :around #'my/zoom-frm-keep-frame-size)
  (advice-add 'zoom-frm-unzoom :around #'my/zoom-frm-keep-frame-size)
  :bind
  ("s-=" . zoom-frm-in)
  ("s--" . zoom-frm-out)
  ("s-0" . zoom-frm-unzoom))

(use-package tramp)

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?_ "w"))))

(use-package swift-mode
  :config
  (setq swift-mode:basic-offset 2))

(use-package dm-evil
             :straight nil)

(use-package dm-projectile :straight nil)

(use-package elscreen
  :init
  (elscreen-start)
  :config
  (setq elscreen-display-screen-number nil)
  (setq elscreen-display-tab 30)
  (setq elscreen-tab-display-control nil)
  (setq elscreen-tab-display-kill-screen nil)
  (defhydra hydra-elscreen (base-leader-map "t")
    "screens"
    ("c" elscreen-create :exit t)
    ("n" elscreen-next)
    ("p" elscreen-previous)
    ("k" elscreen-kill)
    ("j" elscreen-select-and-goto :exit t)
    ("t" elscreen-toggle-display-tab)
    )
  :bind (:map global-map
	 ("s-t" . elscreen-create)
	 ("s-w" . elscreen-kill)
	 ("s-{" . elscreen-previous)
	 ("s-}" . elscreen-next)))

(use-package flx-ido
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  :bind (:map ido-completion-map
	 ("C-k" . kill-whole-line)))

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :init
  (ido-vertical-mode 1))

(add-hook 'after-init-hook (lambda ()
			     (if window-system
				 (server-start))
			     (setq-default abbrev-mode t)
			     (global-hl-line-mode)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables (quote ("PATH" "MANPATH" "NV_DIR" "BASE16_THEME")))
  (exec-path-from-shell-initialize))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map ",," 'evil-avy-goto-char)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "''" 'helm-M-x))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package nlinum
  :config
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (setq nlinum-format " %d"))
  
(use-package nlinum-relative
    :commands (nlinum-relative-on)
    :init
    (setq nlinum-relative-current-symbol ""
	  nlinum-relative-redisplay-delay 0.1)
    (add-hook 'nlinum-mode-hook 'nlinum-relative-on)
    :config
    (nlinum-relative-setup-evil))

;; LOAD ALL THE THINGS
; (dolist (elt (file-expand-wildcards "~/.emacs.d/autoload/*.el"))
;   (load elt))

(use-package which-key
  :config
  (which-key-mode))

(use-package sass-mode)

(use-package dm-magit :straight nil)
(use-package dm-todo :straight nil)
(use-package dm-minibuffer :straight nil)
(use-package dm-ruby :straight nil)
(use-package dm-flycheck :straight nil)
(use-package dm-javascript :straight nil)
(use-package dm-colors :straight nil)
(use-package dm-mode-line :straight nil)
(use-package dm-web-mode :straight nil)
(use-package dm-projectile-rails :straight nil)
(use-package dm-yasnippet :straight nil)
(use-package chord-pro-mode :straight nil)
(use-package dm-org :straight nil)
(use-package dm-prose :straight nil)

(use-package emojify
  :config
  (global-emojify-mode)
  (setq emojify-display-style 'unicode)
  )

(require 'epa-file)
(epa-file-enable)

(use-package origami
             :config 
             (global-origami-mode))

(use-package help+)
(use-package help-fns+)

;; -------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode nil)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("9ab255ab33b11529148334e8c703459fd38f484a28a8f94b219a09fa6efd9a98" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" default)))
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(epg-gpg-program "gpg")
 '(evil-insert-state-modes
   (quote
    (comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode)))
 '(helm-imenu-fuzzy-match t t)
 '(helm-source-names-using-follow
   (quote
    ("Search at ~/Code/groups/" "Search at ~/Kalabox/living/code/" "Jump to: " "global-mark-ring" "mark-ring" "Occur")))
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js2-missing-semi-one-line-override t)
 '(line-number-mode nil)
 '(line-spacing 1)
 '(mac-mouse-wheel-smooth-scroll t)
 '(markdown-asymmetric-header t)
 '(markdown-header-scaling t)
 '(ns-auto-hide-menu-bar t)
 '(ns-command-modifier (quote super))
 '(org-agenda-files (quote ("~/Dropbox/org/agenda.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-mouse org-rmail org-w3m)))
 '(org-pretty-entities t)
 '(org-refile-targets (quote ((org-agenda-files :maxlevel . 3))))
 '(org-todo-keywords (quote ((sequence "TODO(t)" "DONE(d)"))))
 '(package-selected-packages
   (quote
    (magit-org-todos emojify tide typescript-mode org-alert rufo-mode ace-jump-mode winner-mode dumb-jump hydra evil-matchit help-fns+ help+ ruby-end rjsx-mode which-key js-mode use-package rufo pallet flycheck-package org-mobile-sync origami dashboard pinentry sx fish-mode company-sourcekit eslintd-fix php+-mode drupal-mode fzf swift-mode buffer-move ido-other-window ido-completing-read+ ruby-refactor evil-multiedit enh-ruby-mode evil-visualstar lua-mode mwe-log-commands suggest firebelly-theme gruvbox-theme rainbow-delimiters flycheck-elixir-credo markdown-mode flycheck evil-magit 0blayout slim-mode mmm-mode writeroom-mode rainbow-mode browse-at-remote company-mode yasnippet zoom-frm sass-mode emmet-mode alchemist elixir-mode sr-speedbar yaml-mode elscreen web-mode ## helm-dash projectile-rails helm-ag helm-projectile evil-leader projectile evil)))
 '(php-mode-coding-style (quote drupal))
 '(ring-bell-function (quote ignore))
 '(ruby-end-insert-newline nil)
 '(ruby-refactor-add-parens t)
 '(safe-local-variable-values
   (quote
    ((rufo-mode-use-bundler . t)
     (eval setq-local flycheck-disabled-checkers
           (append flycheck-disabled-checkers
                   (quote
                    (ruby-reek))))
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep
                (quote package-build))
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require
                (quote package-build))))
           (package-build-minor-mode)
           (set
            (make-local-variable
             (quote package-build-working-dir))
            (expand-file-name "../working/"))
           (set
            (make-local-variable
             (quote package-build-archive-dir))
            (expand-file-name "../packages/"))
           (set
            (make-local-variable
             (quote package-build-recipes-dir))
            default-directory))
     (projectile-project-type rails-test)
     (projectile-project-type
      (quote rails-test)))))
 '(select-enable-clipboard nil)
 '(show-paren-mode t)
 '(typescript-indent-level 2)
 '(yas-triggers-in-field t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
