;;; dm-lsp.el --- LSP -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(with-eval-after-load 'eglot
  (setopt eglot-extend-to-xref t)
  ;; Deliberately trade JSON-RPC diagnostics for lower per-event overhead.
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs
               (cons '(js-ts-mode
                       typescript-ts-mode
                       tsx-ts-mode
                       typescript-ts-base-mode)
                     (eglot-alternatives
                      '(("typescript-language-server" "--stdio")
                        ("npx" "--no-install" "typescript-language-server" "--stdio"))))))

(use-package eglot-booster
  :disabled
  :after eglot
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))

(use-package lsp-sourcekit
  :disabled
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(provide 'dm-lsp)
;;; dm-lsp.el ends here
