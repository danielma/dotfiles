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

(use-package dm-bindings :ensure nil)

(use-package helm
  :config
  (setq helm-completion-in-region-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-mode-fuzzy-match t
	helm-follow-mode-persistent t)
  (helm-mode)
  :bind (
	 ("C-." . helm-M-x)
	 :map base-leader-map
	 ("<SPC>" . helm-M-x)
	 ("ho" . helm-occur)
	 ("hr" . helm-resume)
	 ("hk" . helm-show-kill-ring))
	 )

(use-package buffer-move
  :commands (buf-move buf-move-right buf-move-left buf-move-up buf-move-down)
  )

(use-package rainbow-delimiters
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  )

(use-package zoom-frm
  :bind
  ("s-=" . zoom-frm-in)
  ("s--" . zoom-frm-out)
  ("s-0" . zoom-frm-unzoom))

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
  (defhydra hydra-elscreen ()
    "screens"
    ("p" elscreen-previous "previous")
    ("n" elscreen-next     "next")
    ("k" elscreen-kill     "kill")
    )
  :bind (:map base-leader-map
	 ("tc" . elscreen-create)
	 ("tn" . elscreen-next)
	 ("tp" . elscreen-previous)
	 ("tk" . elscreen-kill)
	 ("tj" . elscreen-select-and-goto)
	 ("tt" . elscreen-toggle-display-tab)
	 ("t." . hydra-elscreen/body)
	 :map global-map
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
  :init
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

(use-package which-key
  :config
  (which-key-mode))

(use-package markdown-mode)

(use-package dm-magit :ensure nil)
(use-package dm-todo :ensure nil)
(use-package dm-minibuffer :ensure nil)
(use-package dm-ruby :ensure nil)
(use-package dm-flycheck :ensure nil)
(use-package dm-javascript :ensure nil)
(use-package dm-colors :ensure nil)
(use-package dm-mode-line :ensure nil)
(use-package dm-web-mode :ensure nil)
(use-package dm-projectile-rails :ensure nil)
(use-package dm-yasnippet :ensure nil)
(use-package chord-pro-mode :ensure nil)

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
    ("6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" default)))
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(epg-gpg-program "gpg2")
 '(evil-insert-state-modes
   (quote
    (comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode)))
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "NVM_DIR")))
 '(helm-source-names-using-follow (quote ("Occur")))
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(line-number-mode nil)
 '(line-spacing 1)
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
    (try hydra evil-matchit help-fns+ help+ ruby-end rjsx-mode which-key js-mode use-package rufo pallet flycheck-package org-mobile-sync origami dashboard pinentry sx fish-mode company-sourcekit eslintd-fix php+-mode drupal-mode fzf swift-mode buffer-move ido-other-window ido-completing-read+ ruby-refactor evil-multiedit enh-ruby-mode evil-visualstar lua-mode mwe-log-commands suggest firebelly-theme gruvbox-theme rainbow-delimiters flycheck-elixir-credo markdown-mode flycheck evil-magit 0blayout slim-mode mmm-mode writeroom-mode rainbow-mode browse-at-remote company-mode yasnippet zoom-frm sass-mode emmet-mode alchemist elixir-mode sr-speedbar yaml-mode elscreen web-mode ## helm-dash projectile-rails helm-ag helm-projectile evil-leader projectile evil)))
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
 '(writeroom-fullscreen-effect nil)
 '(writeroom-major-modes (quote (markdown-mode)))
 '(yas-triggers-in-field t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
