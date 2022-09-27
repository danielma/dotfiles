;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (js-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . eldoc-box-hover-at-point-mode))
  :commands lsp
  :straight (lsp-mode
             :type git
             :flavor melpa
             :host github
             :repo "emacs-lsp/lsp-mode"
             :files (:defaults "clients/*.el"))
  :custom 
  ;; (lsp-disabled-clients '(ts-ls))
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-lens-enable nil)
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


(provide 'dm-lsp)
