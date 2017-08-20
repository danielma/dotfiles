;; todo: this should open in bottom 20%
(defun custom-flycheck-toggle-errors ()
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (progn
        (delete-window (get-buffer-window (get-buffer "*Flycheck errors*")))
        (kill-buffer "*Flycheck errors*"))
    (flycheck-list-errors)))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d")
  (setq flycheck-command-wrapper-function
        (lambda (command)
          (let ((executable (car command))
                (arguments (cdr command)))
            (if (and (string-suffix-p "rubocop" executable) (locate-dominating-file default-directory ".rubocop.yml"))
                (append '("bundle" "exec" "rubocop") arguments)
              command))))
  (setq-default
    flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint ruby-reek))
    flycheck-temp-prefix ".flycheck")
  (defhydra hydra-flycheck ()
    "flycheck"
    ("l" custom-flycheck-toggle-errors "list")
    ("n" flycheck-next-error           "next")
    ("p" flycheck-previous-error       "previous"))
  :bind (:map base-leader-map
	 ("ll" . custom-flycheck-toggle-errors)
	 ("ln" . flycheck-next-error)
	 ("lp" . flycheck-previous-error)
	 ("l." . hydra-flycheck/body))
  )

(provide 'dm-flycheck)
