;;; dm-ruby.el --- -*- lexical-binding: t -*-

(use-package ruby-mode
  :delight "\ue21e"
  :hook
  (ruby-base-mode . dm-guard-mode))

(if t
    t

  (defun dm/ruby-mode-hook ()
    (setq ruby-insert-encoding-magic-comment nil)
    ;; (modify-syntax-entry ?_ "_")
    )

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
    (add-hook 'ruby-mode-hook 'dm/ruby-mode-hook)
    ;; (add-hook 'ruby-mode-hook #'lsp)
    (add-hook 'ruby-mode-hook 'dm-guard-mode)
    (add-hook 'ruby-mode-hook 'smartparens-mode)
    ;; (add-hook 'ruby-mode-hook 'prettier-js-mode)
    (add-to-list 'auto-mode-alist '("\\.rb.spec\\'" . ruby-mode))
    (add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch))

  (defvar-local my/prettierrc-location-cache nil)

  (defun my/prettierrc-location ()
    "Determine if we should run prettier-js."

    (if (eq my/prettierrc-location-cache 'nope)
        nil
      (or my/prettierrc-location-cache
          (let ((root (locate-dominating-file (or (buffer-file-name) default-directory) ".prettierrc")))
            (setq my/prettierrc-location-cache (or root 'nope))))))

  (defun prettier-js-in-projectile (orig-fun &rest args)
    "Wrap around prettier JS, adding on my own logic for if it should really run.

If it runs, call ORIG-FUN with ARGS."
    (let ((root (my/prettierrc-location))
          (original-directory default-directory))
      (if root
          (progn
            (cd root)
            (unwind-protect
                (apply orig-fun args)
              (cd original-directory)))
        (prettier-js-mode 0))))

  (use-package prettier-js
    :config
    (setq prettier-js-command "prettier_d_slim")
    (advice-add 'prettier-js :around 'prettier-js-in-projectile)
    )

  (use-package ruby-refactor
    :requires bind-map
    :bind
    ;; (:map ruby-mode-map
    ;;       ("ae" . ruby-refactor-extract-to-method)))
    :custom
    (ruby-refactor-add-parens t)
    :config
    (let ((map (make-sparse-keymap)))
      (bind-keys :map map
                 ("e" . ruby-refactor-extract-to-method)
                 ("p" . ruby-refactor-add-parameter)
                 ("l" . ruby-refactor-extract-to-let)
                 ("v" . ruby-refactor-extract-local-variable)
                 ("c" . ruby-refactor-extract-constant)
                 ("o" . ruby-refactor-convert-post-conditional))
      (bind-map-for-mode-inherit my/ruby-refactor-mode-map base-leader-map
                                 :major-modes (ruby-mode)
                                 :bindings ("a" map))))

  (use-package bundler)

  (use-package dm-rspec
    :disabled
    :straight nil
    :config
    (bind-map-for-mode-inherit my/rspec-mode-leader-map base-leader-map
                               :major-modes (rspec-mode)
                               :bindings ("a" rspec-mode-command-map)))
  )

(provide 'dm-ruby)
