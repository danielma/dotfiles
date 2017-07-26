(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and root eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))


(defun flycheck-setup ()
  (flycheck-define-checker javascript-flow
    "A JavaScript syntax and style checker using Flow.
  See URL `http://flowtype.org/'."
    :command ("flow" "check" "--old-output-format" source-original)
    :error-patterns
    ((error line-start
            (file-name)
            ":"
            line
            ":"
            (minimal-match (one-or-more not-newline))
            ": "
            (message (minimal-match (and (one-or-more anything) "\n")))
            line-end))
    :modes (js-mode js2-mode))
  (add-to-list 'flycheck-checkers 'javascript-flow t))

;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(add-hook 'after-init-hook 'flycheck-setup)
;; (add-hook 'flycheck-before-syntax-check-hook #'my/eslint-fix)
(evil-set-initial-state 'flycheck-error-list-mode 'emacs) 
