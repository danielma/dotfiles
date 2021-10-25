;;; Commentary:

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
(setq straight-enable-use-package-integration t
      straight-use-package-by-default t)

(menu-bar-mode 0)
(if (display-graphic-p)
    (progn
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

(use-package emacs
  :custom
  (inhibit-startup-message t)
  (make-backup-files nil)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (frame-resize-pixelwise t))

(add-hook 'after-init-hook (lambda () (if window-system (server-start))))

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'dm-general)
(require 'dm-text)
(require 'dm-bindings)
(require 'dm-completion)

(defun my/custom-dumb-jump-go ()
  "Custom dumb jump command."
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
	 ("sd" . dumb-jump-go)
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

(require 'dm-evil)

(use-package dm-projectile :straight nil)

(use-package dm-projects :straight nil)

(use-package dm-box-drawing :straight nil)

(use-package dm-tabs :straight nil)

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :init
  (ido-vertical-mode 1))

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

(use-package which-key
  :config
  (which-key-mode))

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

;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epa-pinentry-mode 'loopback)

;;; init.el ends here
