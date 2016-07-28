(add-hook 'ruby-mode-hook 'my-ruby-mode-setup)

(defun my-ruby-mode-setup ()
  (hs-minor-mode t)
  (lambda () (modify-syntax-entry ?_ "w"))
  )

(define-abbrev-table 'ruby-mode-abbrev-table '(
                                               ("dsc" "described_class")
                                               ("sbj" "subject")
                                               ("aseq" "assert_equal")
                                               ("ass" "assert")
                                               ("AS::" "ActiveSupport::")
                                               ("AR::" "ActiveRecord::")))
