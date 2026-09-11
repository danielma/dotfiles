;;; dm-lsp.el --- LSP -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(with-eval-after-load 'eglot
  (setopt eglot-extend-to-xref t)
  ;; Deliberately trade JSON-RPC diagnostics for lower per-event overhead.
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs '(swift-mode . ("sourcekit-lsp"))))

(provide 'dm-lsp)
;;; dm-lsp.el ends here
