;;; dm-lsp.el --- LSP -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(with-eval-after-load 'eglot
  (when t ; something needs us to load these compiled versions for eglot to run
    (load "project.elc")
    (load "xref.elc"))
  ;; (add-to-list 'eglot-server-programs '(swift-mode . ("sourcekit-lsp")))
  )

(use-package eglot-booster
  :disabled
  :after eglot
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))

(use-package lsp-sourcekit
  :disabled
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(if t
    nil
  (setq lsp-keymap-prefix "s-l")

  ;; (use-package eldoc-box
  ;;   :delight eldoc-box-hover-at-point-mode)

  (use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
           ;; (js-mode . lsp)
           ;; if you want which-key integration
           (lsp-mode . lsp-enable-which-key-integration)
           ;; (lsp-mode . eldoc-box-hover-at-point-mode))
           )
    :commands lsp
    :straight (lsp-mode
               :type git
               :flavor melpa
               :host github
               :repo "emacs-lsp/lsp-mode"
               :files (:defaults "clients/*.el"))
    :config
    (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.build\\'")
    :custom
    ;; (lsp-disabled-clients '(ts-ls))
    (lsp-headerline-breadcrumb-enable nil)
    (lsp-enable-file-watchers t)
    (lsp-lens-enable nil)
    (lsp-ui-doc-show-with-cursor t)
    (lsp-ui-doc-position 'at-point)
    )

  ;; optionally
  (use-package lsp-ui :commands lsp-ui-mode)
  ;; if you are helm user
  ;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
  ;; if you are ivy user
  (use-package lsp-ivy
    :disabled
    :straight (:type git :host github :repo "emacs-lsp/lsp-ivy")
    :commands lsp-ivy-workspace-symbol)
  ;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

  ;; optionally if you want to use debugger
  ;; (use-package dap-mode)
  ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

  )

(provide 'dm-lsp)
;;; dm-lsp.el ends here
