;;; .emacs --- take care of business
;;; Code:
;;; Commentary:

(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq gc-cons-threshold 20000000)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      make-backup-files nil
      indent-tabs-mode nil
      windmove-wrap-around t
      ns-use-native-fullscreen nil
      ad-redefinition-action 'accept
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      create-lockfiles nil)

(fset 'evil-visual-update-x-selection 'ignore)

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t
      use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'text-tools)
(require 'general-funcs)
(require 'global-map)

;; set all widths to 2
;; (dolist (width '(evil-shift-width))
;;         (set width 2))

(use-package helm
  :config
  (setq helm-completion-in-region-fuzzy-match t
	helm-mode-fuzzy-match t)
  (helm-mode))

(use-package buffer-move
  :commands (buf-move buf-move-right buf-move-left buf-move-up buf-move-down)
  )

(use-package rainbow-delimiters)

; (use-package emacs-lisp
;   :init
;   (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;   :mode ("Cask" . emacs-lisp-mode))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook (lambda () (modify-syntax-entry ?_ "w"))))

(use-package dm-evil
             :ensure nil)

(use-package dm-projectile :ensure nil)

(use-package elscreen
  :init
  (elscreen-start)
  :config
  (setq elscreen-display-screen-number nil)
  (setq elscreen-display-tab 30)
  (setq elscreen-tab-display-control nil)
  (setq elscreen-tab-display-kill-screen nil)
  :bind (:map evil-leader--default-map
	 ("tn" . elscreen-create)
	 ("tl" . elscreen-next)
	 ("th" . elscreen-previous)
	 ("tq" . elscreen-kill)
	 ("tj" . elscreen-select-and-goto)
	 ("tt" . elscreen-toggle-display-tab)
	 :map global-map
	 ("s-t" . elscreen-create)
	 ("s-w" . elscreen-kill)
	 ("s-{" . elscreen-previous)
	 ("s-}" . elscreen-next)))

(use-package flx-ido
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  :config
  (flx-ido-mode 1))

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
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map ",," 'ace-jump-mode)
  (key-chord-define evil-normal-state-map "''" 'helm-M-x))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package nlinum
  :config
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'text-mode-hook 'nlinum-mode)
  (setq nlinum-format " %d"))
  
(use-package nlinum-relative
    :commands (nlinum-relative-on)
    :init
    (setq nlinum-relative-current-symbol ""
	  nlinum-relative-redisplay-delay 0.1)
    (add-hook 'nlinum-mode-hook 'nlinum-relative-on)
    :config
    (nlinum-relative-setup-evil))

(use-package company
  :config
  (global-company-mode)
  (setq company-dabbrev-downcase nil
	company-idle-delay 0.2)
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;; LOAD ALL THE THINGS
; (dolist (elt (file-expand-wildcards "~/.emacs.d/autoload/*.el"))
;   (load elt))

(use-package dm-magit :ensure nil)
(use-package dm-todo :ensure nil)
(use-package dm-minibuffer :ensure nil)
(use-package dm-ruby :ensure nil)
(use-package dm-flycheck :ensure nil)
(use-package dm-javascript :ensure nil)
(use-package dm-colors :ensure nil)
(use-package dm-mode-line :ensure nil)
(use-package dm-web-mode :ensure nil)

(require 'epa-file)
(epa-file-enable)

(use-package origami
             :config 
             (global-origami-mode))

;; -------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector [base00 base08 base0B base0A base0D base0E base0D base05])
 '(ansi-term-color-vector
   [unspecified base00 base08 base0B base0A base0D base0E base0D base05])
 '(column-number-mode nil)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" default)))
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(emmet-indentation 2)
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(epg-gpg-program "gpg2")
 '(evil-echo-state nil)
 '(evil-insert-state-modes
   (quote
    (comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode git-commit-mode)))
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "NVM_DIR")))
 '(flycheck-disabled-checkers (quote (javascript-jshint ruby-reek)))
 '(global-flycheck-mode t)
 '(helm-ag-fuzzy-match t)
 '(helm-ag-insert-at-point (quote symbol))
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(line-number-mode nil)
 '(line-spacing 0)
 '(mac-mouse-wheel-smooth-scroll t)
 '(markdown-asymmetric-header t)
 '(markdown-header-scaling t)
 '(ns-auto-hide-menu-bar t)
 '(ns-command-modifier (quote super))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-mouse org-rmail org-w3m)))
 '(org-pretty-entities t)
 '(org-todo-keywords (quote ((sequence "TODO(t)" "DONE(d)"))))
 '(package-selected-packages
   (quote
    (js-mode use-package rufo pallet flycheck-package org-mobile-sync origami dashboard pinentry sx fish-mode company-sourcekit eslintd-fix php+-mode drupal-mode fzf swift-mode buffer-move ido-other-window magithub ido-completing-read+ ruby-refactor evil-multiedit enh-ruby-mode evil-visualstar lua-mode mwe-log-commands suggest firebelly-theme gruvbox-theme rainbow-delimiters flycheck-elixir-credo markdown-mode flycheck evil-magit 0blayout slim-mode mmm-mode writeroom-mode rainbow-mode browse-at-remote company-mode yasnippet zoom-frm sass-mode emmet-mode alchemist elixir-mode sr-speedbar yaml-mode elscreen web-mode ## helm-dash projectile-rails helm-ag helm-projectile evil-leader projectile evil)))
 '(projectile-completion-system (quote ido))
 '(projectile-generic-command "ag -g \"\"")
 '(projectile-global-mode t)
 '(projectile-switch-project-action (quote projectile-dired))
 '(ruby-end-insert-newline nil)
 '(ruby-refactor-add-parens t)
 '(safe-local-variable-values
   (quote
    ((eval setq-local flycheck-disabled-checkers
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
 '(term-scroll-show-maximum-output t)
 '(term-scroll-to-bottom-on-output t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2)
 '(writeroom-fullscreen-effect nil)
 '(writeroom-major-modes (quote (markdown-mode)))
 '(yas-triggers-in-field t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
