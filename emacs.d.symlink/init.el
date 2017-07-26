;;; -*- mode: emacs-lisp -*-
;;; .emacs --- take care of business
;;; Code:
;;; Commentary:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq gc-cons-threshold 20000000)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      make-backup-files nil
      byte-compile-warnings '(not free-vars)
      indent-tabs-mode nil
      windmove-wrap-around t
      ad-redefinition-action 'accept)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;; set all widths to 2
;; (dolist (width '(evil-shift-width))
;;         (set width 2))

(defun interactive-wrap-with-pair (pair)
  (interactive "c")
  (sp-wrap-with-pair (char-to-string pair)))

(defun expand-at-point ()
  "Insert a newline and put the cursor at the indented location above."
  (interactive)
  (newline-and-indent)
  (evil-open-above 1))

(use-package evil
  :init
  (setq evil-shift-width 2)
  :config
  (evil-mode 1)
  :bind (:map evil-insert-state-map
	 ("C-n" . next-line)
	 ("C-p" . previous-line)
	 ("C-a" . beginning-of-line-text)
	 ("C-e" . end-of-line)
	 ("M-RET" . expand-at-point)
	 :map evil-normal-state-map
	 ("M-RET" . newline)
	 ("[b" . previous-buffer)
	 ("]b" . next-buffer)
	 ("C-." . helm-M-x)
	 :map evil-visual-state-map
	 ("C-w" . interactive-wrap-with-pair)
	 ))

(use-package elscreen
  :config
  (setq elscreen-display-screen-number nil)
  (setq elscreen-display-tab 30)
  (setq elscreen-tab-display-control nil)
  (setq elscreen-tab-display-kill-screen nil)
  :init
  (elscreen-start))

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(setq ns-use-native-fullscreen nil)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(load (expand-file-name "~/.dotfiles/emacs.d.symlink/text-tools.el"))

(add-hook 'after-init-hook (lambda ()
                             (when (memq window-system '(mac ns))
                               (exec-path-from-shell-initialize))
                             ))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map ",," 'ace-jump-mode)
  (key-chord-define evil-normal-state-map "''" 'helm-M-x))

(use-package evil-visualstar
  :init
  (global-evil-visualstar-mode))

(use-package nlinum
  :init
  (nlinum-relative-setup-evil)
  :config
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'text-mode-hook 'nlinum-mode)
  (setq nlinum-format " %d"))
  
(use-package nlinum-relative
    :commands (nlinum-relative-on)
    :config
    (setq nlinum-relative-current-symbol ""
	  nlinum-relative-redisplay-delay 0)
    :init
    (nlinum-relative-setup-evil)
    (add-hook 'nlinum-mode-hook 'nlinum-relative-on))

(use-package company
  :init
  (global-company-mode)
  :bind (:map company-active-map
              ("M-n" . nil)
              ("M-p" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;; LOAD ALL THE THINGS
(dolist (elt (file-expand-wildcards "~/.emacs.d/autoload/*.el"))
  (load elt))

(setq-default abbrev-mode t)

(global-hl-line-mode)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq create-lockfiles nil)

;;; esc always quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(require 'epa-file)
(epa-file-enable)

(global-origami-mode)

;; (with-eval-after-load "common-header-mode-line-autoloads"
;;   (add-hook
;;    'after-init-hook
;;    #'(lambda ()
;;        (common-header-line-mode 1)
;;        (setq per-frame-mode-line-update-display-function
;;                  #'(lambda (display)
;;                      (let ((buf (cdr (assq 'buf display))))
;;                        (with-current-buffer buf
;;                          (setq-local buffer-read-only nil)
;;                          (erase-buffer)
;;                          (let*
;;                              ((mode-l-str
;;                                (format-mode-line
;;                                 `("%e" mode-line-front-space
;;                                   (eldoc-mode-line-string (" " eldoc-mode-line-string " "))
;;                                   mode-line-modified mode-line-client mode-line-frame-identification
;;                                   mode-line-modes mode-line-misc-info mode-line-end-spaces)
;;                                 'per-frame-mode-line-face per-frame-header-mode-line--selected-window)))
;;                            (insert mode-l-str))
;;                          (setq-local mode-line-format nil)
;;                          (setq-local header-line-format nil)
;;                          (goto-char (point-min))
;;                          (setq-local buffer-read-only t))))))))

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
 '(company-dabbrev-downcase nil)
 '(company-idle-delay 0.2)
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
 '(evil-shift-round t)
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "NVM_DIR")))
 '(flycheck-disabled-checkers (quote (javascript-jshint ruby-reek)))
 '(global-flycheck-mode t)
 '(helm-ag-fuzzy-match t)
 '(helm-ag-insert-at-point (quote symbol))
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-mode-fuzzy-match t)
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(line-number-mode nil)
 '(line-spacing 0)
 '(mac-mouse-wheel-smooth-scroll t)
 '(magit-bury-buffer-function (quote magit-mode-quit-window))
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-log-arguments (quote ("-n20" "--graph" "--decorate")))
 '(magit-log-select-arguments (quote ("-n20" "--decorate")))
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-save-repository-buffers nil)
 '(magithub-api-timeout 6)
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
    (use-package rufo pallet flycheck-package org-mobile-sync origami dashboard pinentry sx fish-mode company-sourcekit eslintd-fix php+-mode drupal-mode fzf swift-mode buffer-move ido-other-window magithub ido-completing-read+ ruby-refactor evil-multiedit enh-ruby-mode evil-visualstar lua-mode mwe-log-commands suggest firebelly-theme gruvbox-theme rainbow-delimiters flycheck-elixir-credo markdown-mode flycheck evil-magit 0blayout slim-mode mmm-mode writeroom-mode rainbow-mode browse-at-remote company-mode yasnippet zoom-frm sass-mode emmet-mode alchemist elixir-mode sr-speedbar yaml-mode elscreen web-mode ## helm-dash projectile-rails helm-ag helm-projectile evil-leader projectile evil)))
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
     (rufo-mode-use-bundler . t)
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
