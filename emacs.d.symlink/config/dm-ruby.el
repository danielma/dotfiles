(use-package rufo-mode
  :init
  (add-hook 'ruby-mode-hook 'rufo-minor-mode)
  :defer t)

(use-package ruby-mode
  :config
  (define-abbrev-table 'ruby-mode-abbrev-table '(
						 ("dsc" "described_class")
						 ("sbj" "subject")
						 ("aseq" "assert_equal")
						 ("aspd" "assert_predicate")
						 ("ass" "assert")
						 ("AS::" "ActiveSupport::")
						 ("AR::" "ActiveRecord::")))
  :init
  (add-hook 'ruby-mode-hook (lambda ()
			      (setq ruby-insert-encoding-magic-comment nil)
			      (modify-syntax-entry ?_ "w")
			      ))
  (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

(use-package ruby-refactor
  :init
  (evil-leader/set-key-for-mode 'ruby-mode
    "o" (let ((map (make-sparse-keymap)))
	  (define-key map (kbd "e") 'ruby-refactor-extract-to-method)
	  (define-key map (kbd "p") 'ruby-refactor-add-parameter)
	  (define-key map (kbd "l") 'ruby-refactor-extract-to-let)
	  (define-key map (kbd "v") 'ruby-refactor-extract-local-variable)
	  (define-key map (kbd "c") 'ruby-refactor-extract-constant)
	  (define-key map (kbd "o") 'ruby-refactor-convert-post-conditional)
	  map)))

(provide 'dm-ruby)
