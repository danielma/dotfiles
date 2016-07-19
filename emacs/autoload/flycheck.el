(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and root eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun flycheck-setup ()
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq-default
    flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint))
    flycheck-temp-prefix ".flycheck")
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

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
(add-hook 'after-init-hook 'flycheck-setup)
