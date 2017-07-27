(use-package rufo-mode
  :init
  (add-hook 'ruby-mode-hook 'rufo-minor-mode)
  :config
  (setq rufo-mode-user-bundler t)
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
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(use-package ruby-end)

(use-package ruby-refactor
  :config
  (eval
   `(bind-map my/ruby-refactor-mode-map
      :keys ,(my/leader-sub-key "o")
      :evil-keys ,(my/leader-evil-sub-key "o")
      :major-modes (ruby-mode)
      :bindings (
		 "e" 'ruby-refactor-extract-to-method
		 "p" 'ruby-refactor-add-parameter
		 "l" 'ruby-refactor-extract-to-let
		 "v" 'ruby-refactor-extract-local-variable
		 "c" 'ruby-refactor-extract-constant
		 "o" 'ruby-refactor-convert-post-conditional
		 ))))

(provide 'dm-ruby)
