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

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-enable-use-package-integration t)

(menu-bar-mode 0)
(if (display-graphic-p)
    (progn
      (menu-bar-mode 1)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

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

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

(fset 'evil-visual-update-x-selection 'ignore)

;(eval-when-compile
(require 'use-package)
;)
(setq use-package-verbose t
      straight-use-package-by-default t)

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;(progn
;;  (add-to-list 'load-path "~/Code/test/emacs-libvterm/")
;;  (let (vterm-install)
;;    (require 'vterm)))

(require 'text-tools)
(require 'general-funcs)
(require 'global-map)

;; set all widths to 2
;; (dolist (width '(evil-shift-width))
;;         (set width 2))

(use-package dm-bindings :straight nil)

(use-package dm-completion :straight nil)

(defun my/custom-dumb-jump-go ()
  (interactive)
  (if (eq major-mode 'typescript-mode)
      (tide-jump-to-definition)
    (dumb-jump-go-prefer-external)))

(use-package dumb-jump
  :config
  (setq dumb-jump-selector 'ivy
        dumb-jump-aggressive t)
  :bind (:map base-leader-map
	 ("sa" . my/custom-dumb-jump-go)
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
  (setq frame-inhibit-implied-resize t)
  (face-spec-recalc 'default (selected-frame))
  :bind
  ("s-=" . zoom-all-frames-in)
  ("s--" . zoom-all-frames-out)
  ("s-0" . zoom-frm-unzoom))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?_ "w"))))

(use-package swift-mode
  :config
  (setq swift-mode:basic-offset 2))

(use-package dm-evil
             :straight nil)

(use-package dm-projectile :straight nil)

(use-package dm-projects :straight nil)

(use-package dm-box-drawing :straight nil)

(use-package dm-tabs :straight nil)

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :init
  (ido-vertical-mode 1))

;; (add-hook 'after-init-hook (lambda ()
;; 			     (if window-system
;; 				 (server-start))
;; 			     (global-hl-line-mode)))

(use-package abbrev
  :straight nil
  :config
  (define-abbrev
    global-abbrev-table "orgn" "organization")
  (define-abbrev
    global-abbrev-table "orgns" "organizations")
  :custom
  (abbrev-mode t)
  (save-abbrevs nil)
  )

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables (quote ("PATH" "MANPATH" "BASE16_THEME")))
  (exec-path-from-shell-initialize))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map ",," 'evil-avy-goto-char)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-normal-state-map "''" 'my/m-x))

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

;; (use-package tree-sitter)
;; (use-package tree-sitter-langs)

(use-package dm-magit :straight nil)
(use-package dm-todo :straight nil)
(use-package dm-minibuffer :straight nil)
(use-package dm-guard :straight nil)
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
(use-package dm-lsp :straight nil)

(use-package rustic
  :config
  (setq rustic-format-on-save t
        rustic-format-display-method 'display-buffer))

(use-package emojify
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode))
  )

(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)

(use-package origami
             :config 
             (global-origami-mode))

(use-package help+ :disabled)
(use-package help-fns+ :disabled)

;; -------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode nil)
 '(css-indent-offset 2)
 '(custom-safe-themes
   '("1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "0ab2aa38f12640ecde12e01c4221d24f034807929c1f859cbca444f7b0a98b3a" "39e1d15a202583b1ec9ca98e7e54252f717bd6012cd5f4285ed153921b66cdfd" "13d390ab4e50fda339b620327790893dc8adab530ba4791ca44ba68f538d71c5" "f000ac0d2b8ecdbe47c0db98610d6ed8c9ba9185af75bd54232c9b5374372ef3" "9ab255ab33b11529148334e8c703459fd38f484a28a8f94b219a09fa6efd9a98" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" default))
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(epg-gpg-program "gpg")
 '(evil-insert-state-modes
   '(comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode))
 '(highlight-indent-guides-method 'character)
 '(indent-tabs-mode nil)
 '(initial-scratch-message nil)
 '(ivy-on-del-error-function 'ignore)
 '(js-indent-level 2)
 '(js2-missing-semi-one-line-override t)
 '(line-number-mode nil)
 '(line-spacing 0.2)
 '(mac-mouse-wheel-smooth-scroll nil)
 '(markdown-asymmetric-header t)
 '(markdown-header-scaling t)
 '(ns-auto-hide-menu-bar t)
 '(ns-command-modifier 'super)
 '(org-agenda-files '("~/Dropbox/org/agenda.org"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-mouse org-rmail org-w3m))
 '(org-pretty-entities t)
 '(org-refile-targets '((org-agenda-files :maxlevel . 3)) t)
 '(org-todo-keywords '((sequence "TODO(t)" "DONE(d)")))
 '(package-selected-packages
   '(emojify tide typescript-mode org-alert rufo-mode ace-jump-mode winner-mode dumb-jump hydra evil-matchit help-fns+ help+ ruby-end rjsx-mode which-key js-mode use-package rufo pallet flycheck-package org-mobile-sync origami dashboard pinentry sx fish-mode company-sourcekit eslintd-fix php+-mode drupal-mode fzf swift-mode buffer-move ido-other-window ido-completing-read+ ruby-refactor evil-multiedit enh-ruby-mode evil-visualstar lua-mode mwe-log-commands suggest firebelly-theme gruvbox-theme rainbow-delimiters flycheck-elixir-credo markdown-mode flycheck evil-magit 0blayout slim-mode mmm-mode writeroom-mode rainbow-mode browse-at-remote company-mode yasnippet zoom-frm sass-mode emmet-mode alchemist elixir-mode sr-speedbar yaml-mode elscreen web-mode ## helm-dash projectile-rails helm-ag helm-projectile evil-leader projectile evil))
 '(php-mode-coding-style 'drupal)
 '(ring-bell-function 'ignore)
 '(ruby-end-insert-newline nil)
 '(ruby-refactor-add-parens t)
 '(safe-local-variable-values
   '((prettier-js-args "--plugin" "/Users/danielma/.config/yarn/global/node_modules/@prettier/plugin-ruby")
     (prettier-js-args "--plugin" "~/.config/yarn/global/node_modules/@prettier/plugin-ruby")
     (prettier-js-args "--plugin ~/.config/yarn/global/node_modules/@prettier/plugin-ruby")
     (checkdoc-package-keywords-flag)
     (prettier-js-args "exec" "rbprettier")
     (prettier-js-command . "bundle")
     (prettier-js-command . "prettier_d")
     (eval push 'ruby-rubocop flycheck-disabled-checkers)
     (eval push "ruby-rubocop" flycheck-disabled-checkers)
     (eval push "ruby-rubocop")
     (rufo-mode-use-bundler . t)
     (eval when
           (and
            (buffer-file-name)
            (file-regular-p
             (buffer-file-name))
            (string-match-p "^[^.]"
                            (buffer-file-name)))
           (unless
               (featurep 'package-build)
             (let
                 ((load-path
                   (cons "../package-build" load-path)))
               (require 'package-build)))
           (package-build-minor-mode)
           (set
            (make-local-variable 'package-build-working-dir)
            (expand-file-name "../working/"))
           (set
            (make-local-variable 'package-build-archive-dir)
            (expand-file-name "../packages/"))
           (set
            (make-local-variable 'package-build-recipes-dir)
            default-directory))
     (projectile-project-type rails-test)
     (projectile-project-type 'rails-test)))
 '(select-enable-clipboard nil)
 '(selectric-mode nil)
 '(show-paren-mode t)
 '(typescript-indent-level 2)
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:weight normal :height 120 :width normal :family "JetBrains Mono")))))
(put 'downcase-region 'disabled nil)
