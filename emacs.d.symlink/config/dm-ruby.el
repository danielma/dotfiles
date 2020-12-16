(use-package rufo
  :init
  (add-hook 'ruby-mode-hook 'rufo-minor-mode)
  :config
  (setq xrufo-mode-use-bundler t)
  :disabled)

(use-package ruby-mode
  :init
  (define-abbrev-table 'ruby-mode-abbrev-table '(
						 ("dsc" "described_class")
						 ("sbj" "subject")
						 ("aseq" "assert_equal")
						 ("aspd" "assert_predicate")
						 ("ass" "assert")
						 ("AS::" "ActiveSupport::")
						 ("AR::" "ActiveRecord::")))
  (add-hook 'ruby-mode-hook (lambda ()
			      (setq ruby-insert-encoding-magic-comment nil)
			      (modify-syntax-entry ?_ "w")
			      ))
  ;; (add-hook 'ruby-mode-hook #'lsp)
  (add-hook 'ruby-mode-hook 'dm-guard-mode)
  (add-hook 'ruby-mode-hook 'smartparens-mode)
  (add-to-list 'auto-mode-alist '("\\.rb.spec\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(defun prettier-js-in-projectile (orig-fun &rest args)
  (if (projectile-project-p)
      (let ((original-directory default-directory))
        (message original-directory)
        (cd (projectile-project-root))
        (apply orig-fun args)
        (cd original-directory))
    (apply orig-fun args)))

(use-package prettier-js
  :config
  (setq prettier-js-command "prettier_d")
  (advice-add 'prettier-js :around 'prettier-js-in-projectile)
  )

(use-package ruby-end)

(use-package ruby-refactor
  :config
  (let ((map (make-sparse-keymap)))
    (bind-map-set-keys map
      "e" 'ruby-refactor-extract-to-method
      "p" 'ruby-refactor-add-parameter
      "l" 'ruby-refactor-extract-to-let
      "v" 'ruby-refactor-extract-local-variable
      "c" 'ruby-refactor-extract-constant
      "o" 'ruby-refactor-convert-post-conditional)
    (bind-map-for-mode-inherit my/ruby-refactor-mode-map base-leader-map
      :major-modes (ruby-mode)
      :bindings ("a" map))))

(use-package dm-rspec
  :straight nil
  :config
  (bind-map-for-mode-inherit my/rspec-mode-leader-map base-leader-map
    :major-modes (rspec-mode)
    :bindings ("a" rspec-mode-command-map)))

(provide 'dm-ruby)
