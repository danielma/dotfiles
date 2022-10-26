;;; init.el --- Main config -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-enable-use-package-integration t
      straight-use-package-by-default t
      use-package-always-demand t)

(if (display-graphic-p)
    (progn
      ;; (set-frame-parameter (selected-frame) 'undecorated nil)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

(menu-bar-mode 0)

(use-package emacs
  :custom
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024)) ;; 1mb

  (inhibit-startup-message t)
  (make-backup-files nil)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t))

(add-hook 'after-init-hook (lambda () (if window-system (server-start))))

(add-to-list 'load-path "~/.emacs.d/config/")
;; (add-to-list 'load-path "~/.emacs.d/lisp/")

(defvar base-leader-map (make-sparse-keymap) "The main LEADER map.")

(require 'dm-general)

(require 'dm-bindings)
(require 'dm-colors)
(require 'dm-completion)
(require 'dm-evil)
(require 'dm-flycheck)
(require 'dm-guard)
(require 'dm-javascript)
(require 'dm-langs)
(require 'dm-magit)
(require 'dm-mode-line)
(require 'dm-prog)
(require 'dm-projects)
(require 'dm-ruby)
(require 'dm-tabs)
(require 'dm-text)
(require 'dm-ui)
(require 'dm-web-mode)
(require 'dm-yasnippet)

;; (use-package dm-projectile :straight nil)

;; (use-package dm-box-drawing :straight nil)

;; (require 'dm-langs)

;; (use-package dm-magit :straight nil)
;; (use-package dm-todo :straight nil)
;; (use-package dm-minibuffer :straight nil)
;; (use-package dm-guard :straight nil)
;; (use-package dm-ruby :straight nil)
;; (use-package dm-flycheck :straight nil)
;; (use-package dm-javascript :straight nil)
;; (use-package dm-colors :straight nil)
;; (use-package dm-mode-line :straight nil)
;; (use-package dm-web-mode :straight nil)
;; (use-package dm-projectile-rails :straight nil)
;; (use-package dm-yasnippet :straight nil)
;; (use-package chord-pro-mode :straight nil)
;; (use-package dm-org :straight nil)
;; (use-package dm-prose :straight nil)
;; (use-package dm-lsp :straight nil)

;; (require 'epa-file)
;; (epa-file-enable)
;; (setq epa-pinentry-mode 'loopback)

;; only for testing
(find-file "~/.emacs.d/init.el")

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources '("~/.netrc" macos-keychain-internet))
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "70b596389eac21ab7f6f7eb1cf60f8e60ad7c34ead1f0244a577b1810e87e58c" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "8a379e7ac3a57e64de672dd744d4730b3bdb88ae328e8106f95cd81cbd44e0b6" "2035a16494e06636134de6d572ec47c30e26c3447eafeb6d3a9e8aee73732396" "ba72dfc6bb260a9d8609136b9166e04ad0292b9760a3e2431cf0cd0679f83c3a" "41098e2f8fa67dc51bbe89cce4fb7109f53a164e3a92356964c72f76d068587e" "05626f77b0c8c197c7e4a31d9783c4ec6e351d9624aa28bc15e7f6d6a6ebd926" "48972c5f4632da4af89aae7033d96b9dfb146772a85982d728b1c94b73aefca7" "5a89135eeeb295b29a8933ed72dc59e865ef6947ed077c81398755bbcac2d13e" "58dd91a167c57302f8d82abe292d8f78f0d5ba37bdf2f87caec3b49a1a661f06" "344aa68121312e4bbbf8860e029398635c8b72441a90e1135965f85296eeaef8" "dc55ef4d2198c3b01693ffa5e49f65a7738958fb22078de08615722a686eb4d9" "2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "5e769f0dc4c262f216c2a30ca8bf55ff2ebc164f779bd2f32ce989290dc13485" "e624f013e266f41148aa2e445a4b8681b0afb346a9126993e345309c9a829535" "32398e365c1603f22062cb92b085fa11e6a280cdb67f591ec656c3f9899445ae" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "cb4da6642968d84d530ba84113b4e716b71b77496db01afb799280eccc8a81e1" "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" default))
 '(ediff-make-buffers-readonly-at-startup t)
 '(ediff-merge-split-window-function 'split-window-vertically)
 '(line-number-mode nil)
 '(safe-local-variable-values
   '((lsp-enabled-clients ruby-syntax-tree-ls)
     (lsp-enabled-clients quote
                          (ruby-syntax-tree-ls))
     (eval when
           (fboundp 'rainbow-mode)
           (rainbow-mode 1)))))
(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :width regular :weight regular :family "JetBrains Mono"))))
 '(tab-bar ((t :inherit default :box (:line-width (0 . 8))))))
