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
      ;; (set-frame-parameter (selected-frame) 'undecorated nil)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))

(use-package emacs
  :custom
;;  (gc-cons-threshold 100000000)
;;  (read-process-output-max (* 1024 1024)) ;; 1mb
  (inhibit-startup-message t)
  (make-backup-files nil)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
  (ring-bell-function 'ignore)
  (frame-resize-pixelwise t))

(add-hook 'after-init-hook (lambda () (if window-system (server-start))))

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

(defvar base-leader-map (make-sparse-keymap) "The main LEADER map.")

(require 'dm-general)
(require 'dm-text)
(require 'dm-bindings)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (setq exec-path-from-shell-variables (quote ("PATH" "MANPATH" "BASE16_THEME")))
  (exec-path-from-shell-initialize))

(require 'dm-completion)
(require 'dm-ui)

(require 'dm-evil)

(use-package dm-projectile :straight nil)

(use-package dm-projects :straight nil)

(use-package dm-box-drawing :straight nil)

(use-package dm-tabs :straight nil)

(require 'dm-langs)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2ea9afebc23cca3cd0cd39943b8297ce059e31cb62302568b8fa5c25a22db5bc" "cdc2a7ba4ecf0910f13ba207cce7080b58d9ed2234032113b8846a4e44597e41" "5e769f0dc4c262f216c2a30ca8bf55ff2ebc164f779bd2f32ce989290dc13485" "e624f013e266f41148aa2e445a4b8681b0afb346a9126993e345309c9a829535" "32398e365c1603f22062cb92b085fa11e6a280cdb67f591ec656c3f9899445ae" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "e9d47d6d41e42a8313c81995a60b2af6588e9f01a1cf19ca42669a7ffd5c2fde" "cb4da6642968d84d530ba84113b4e716b71b77496db01afb799280eccc8a81e1" "95b0bc7b8687101335ebbf770828b641f2befdcf6d3c192243a251ce72ab1692" default))
 '(safe-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
