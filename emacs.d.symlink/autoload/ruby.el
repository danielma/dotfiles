(require 'rufo-mode)

(add-hook 'ruby-mode-hook 'my-ruby-mode-setup)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
;; (add-hook 'ruby-mode-hook 'rufo-mode)

(defun my-ruby-mode-setup ()
  (setq ruby-insert-encoding-magic-comment nil)
  (modify-syntax-entry ?_ "w")
  )

(setq my/ruby-refactor-mode-command-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "e") 'ruby-refactor-extract-to-method)
        (define-key map (kbd "p") 'ruby-refactor-add-parameter)
        (define-key map (kbd "l") 'ruby-refactor-extract-to-let)
        (define-key map (kbd "v") 'ruby-refactor-extract-local-variable)
        (define-key map (kbd "c") 'ruby-refactor-extract-constant)
        (define-key map (kbd "o") 'ruby-refactor-convert-post-conditional)
        map))

(evil-define-minor-mode-key
  'normal
  'ruby-refactor-mode
  (kbd (concat evil-leader/leader "o"))
  my/ruby-refactor-mode-command-map)

(evil-define-minor-mode-key
  'visual
  'ruby-refactor-mode
  (kbd (concat evil-leader/leader "o"))
  my/ruby-refactor-mode-command-map)

(define-abbrev-table 'ruby-mode-abbrev-table '(
                                               ("dsc" "described_class")
                                               ("sbj" "subject")
                                               ("aseq" "assert_equal")
                                               ("aspd" "assert_predicate")
                                               ("ass" "assert")
                                               ("AS::" "ActiveSupport::")
                                               ("AR::" "ActiveRecord::")))
