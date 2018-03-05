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
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
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

  ;; https://github.com/abo-abo/hydra/wiki/Flycheck
  (defhydra hydra-flycheck
    (base-leader-map "l"
		     :pre (progn (setq hydra-lv t) (flycheck-list-errors))
		     :post (progn (setq hydra-lv nil) (quit-windows-on "*Flycheck errors*"))
		     :hint nil)
    "Errors"
    ("f"  flycheck-error-list-set-filter                            "Filter")
    ("j"  flycheck-next-error                                       "Next")
    ("k"  flycheck-previous-error                                   "Previous")
    ("gg" flycheck-first-error                                      "First")
    ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
    ("q"  nil))
  :bind (:map base-leader-map
	 ("ll" . hydra-flycheck/body))
  )

(provide 'dm-flycheck)
