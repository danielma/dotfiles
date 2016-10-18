(add-hook 'ruby-mode-hook 'my-ruby-mode-setup)

(defun my-ruby-mode-setup ()
  (hs-minor-mode t)
  (setq ruby-insert-encoding-magic-comment nil)
  (lambda () (modify-syntax-entry ?_ "w"))
  )

(add-to-list 'hs-special-modes-alist
             '(ruby-mode
               "\\(def\\|do\\)" "\\(end\\|end\\)" "#"
               (lambda (arg) (ruby-end-of-block)) nil))
(define-abbrev-table 'ruby-mode-abbrev-table '(
                                               ("dsc" "described_class")
                                               ("sbj" "subject")
                                               ("aseq" "assert_equal")
                                               ("ass" "assert")
                                               ("AS::" "ActiveSupport::")
                                               ("AR::" "ActiveRecord::")))
