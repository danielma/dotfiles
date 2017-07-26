(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (setq flycheck-command-wrapper-function
        (lambda (command)
          (let ((executable (car command))
                (arguments (cdr command)))
            (if (string-suffix-p "rubocop" executable)
                (append '("bundle" "exec" "rubocop") arguments)
              command))))
  (setq-default
    flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint ruby-reek))
    flycheck-temp-prefix ".flycheck")
  )

(provide 'dm-flycheck)
