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

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      make-backup-files nil
      byte-compile-warnings '(not free-vars)
      ad-redefinition-action 'accept)

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'evil)
(evil-mode 1)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
 
(elscreen-start)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)
(setq ns-use-native-fullscreen nil)

(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

(load (expand-file-name "~/.dotfiles/emacs.d.symlink/text-tools.el"))

;; LOAD ALL THE THINGS
(dolist (elt (file-expand-wildcards "~/.emacs.d/autoload/*.el"))
  (load elt))

(setq gc-cons-threshold 20000000)

;;; COMPANY

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-sourcekit)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(setq windmove-wrap-around t)
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

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook (lambda ()
			     (global-nlinum-mode 1)
			     (global-nlinum-relative-mode)
			     (nlinum-relative-setup-evil)))

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
 '(compilation-message-face (quote default))
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" default)))
 '(diff-hl-margin-mode t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(elscreen-display-screen-number nil)
 '(elscreen-display-tab 30)
 '(elscreen-prefix-key (kbd "C-c e"))
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil)
 '(emmet-indentation 2)
 '(enh-ruby-add-encoding-comment-on-save nil)
 '(epg-gpg-program "gpg2")
 '(evil-echo-state nil)
 '(evil-insert-state-modes
   (quote
    (comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode git-commit-mode)))
 '(evil-shift-round t)
 '(evil-shift-width 2)
 '(flycheck-disabled-checkers (quote (javascript-jshint ruby-reek)))
 '(global-flycheck-mode t)
 '(helm-ag-fuzzy-match t)
 '(helm-ag-insert-at-point (quote symbol))
 '(helm-completion-in-region-fuzzy-match t)
 '(helm-mode-fuzzy-match t)
 '(helm-prevent-escaping-from-minibuffer nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(line-number-mode nil)
 '(line-spacing 0)
 '(mac-mouse-wheel-smooth-scroll t)
 '(magit-bury-buffer-function (quote magit-mode-quit-window))
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-diff-use-overlays nil)
 '(magit-log-arguments (quote ("-n20" "--graph" "--decorate")))
 '(magit-log-select-arguments (quote ("-n20" "--decorate")))
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-save-repository-buffers nil)
 '(magithub-api-timeout 6)
 '(markdown-asymmetric-header t)
 '(markdown-header-scaling t)
 '(nlinum-format " %d")
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
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
    (rufo pallet flycheck-package org-mobile-sync origami dashboard pinentry sx fish-mode company-sourcekit eslintd-fix php+-mode drupal-mode fzf swift-mode buffer-move ido-other-window magithub ido-completing-read+ ruby-refactor evil-multiedit enh-ruby-mode evil-visualstar lua-mode mwe-log-commands suggest firebelly-theme gruvbox-theme rainbow-delimiters flycheck-elixir-credo markdown-mode flycheck evil-magit 0blayout slim-mode mmm-mode writeroom-mode rainbow-mode browse-at-remote company-mode yasnippet zoom-frm sass-mode emmet-mode alchemist elixir-mode sr-speedbar yaml-mode elscreen web-mode ## helm-dash projectile-rails helm-ag helm-projectile evil-leader projectile evil)))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(projectile-completion-system (quote ido))
 '(projectile-generic-command "ag -g \"\"")
 '(projectile-global-mode t)
 '(projectile-mode-line (quote Ⓟ))
 '(projectile-switch-project-action (quote projectile-dired))
 '(rm-blacklist
   (quote
    (" hl-p" "company" "yas" "Projectile" "ing" "LR" "Undo-Tree" "Abbrev" "hs")))
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
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(sml/directory-truncation-string "")
 '(sml/mode-width (quote full))
 '(sml/modified-char "*")
 '(sml/replacer-regexp-list
   (quote
    (("^~/org/" ":Org:")
     ("^~/\\.emacs\\.d/elpa/" ":ELPA:")
     ("^~/\\.emacs\\.d/" ":ED:")
     ("^/sudo:.*:" ":SU:")
     ("^~/Documents/" ":Doc:")
     ("^~/Dropbox/" ":DB:")
     ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
     ("^~/[Gg]it/" ":Git:")
     ("^~/[Gg]it[Hh]ub/" ":Git:")
     ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
     ("~/Code/([^/]+)" ":C:"))))
 '(speedbar-show-unknown-files t)
 '(sx-search-default-order (quote relevance))
 '(telephone-line-primary-left-separator (quote telephone-line-abs-left))
 '(telephone-line-primary-right-separator (quote telephone-line-abs-right))
 '(telephone-line-secondary-left-separator (quote telephone-line-abs-hollow-left))
 '(telephone-line-secondary-right-separator (quote telephone-line-abs-hollow-right))
 '(term-scroll-show-maximum-output t)
 '(term-scroll-to-bottom-on-output t)
 '(typescript-indent-level 2)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(web-mode-attr-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(web-mode-sql-indent-offset 2)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(writeroom-fullscreen-effect nil)
 '(writeroom-major-modes (quote (markdown-mode)))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yas-triggers-in-field t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
