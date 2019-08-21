;; todo: this should open in bottom 20%
(defun custom-flycheck-toggle-errors ()
  (interactive)
  (if (get-buffer "*Flycheck errors*")
      (progn
        (delete-window (get-buffer-window (get-buffer "*Flycheck errors*")))
        (kill-buffer "*Flycheck errors*"))
    (flycheck-list-errors)))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (setq-local flycheck-javascript-eslint-executable "eslint_d"))
  ;; (let* ((root (locate-dominating-file
  ;;               (or (buffer-file-name) default-directory)
  ;;               "node_modules"))
  ;;        (eslint (and root
  ;;                     (expand-file-name "node_modules/.bin/eslint"
  ;;                                       root))))
  ;;   (when (and eslint (file-executable-p eslint))
  ;;     (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-rubocop-from-bin ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "Gemfile"))
         (rubocop (and root
                       (expand-file-name "bin/rubocop" root))))
    (when (and rubocop (file-executable-p rubocop))
      (setq-local flycheck-ruby-rubocop-executable rubocop))))

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (add-hook 'flycheck-mode-hook #'my/use-rubocop-from-bin)
  (setq flycheck-javascript-eslint-executable nil)
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default
    flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint ruby-reek))
    flycheck-temp-prefix ".flycheck")

  ;; https://github.com/abo-abo/hydra/wiki/Flycheck
  (defhydra hydra-flycheck
    (base-leader-map "l"
		     :pre (flycheck-list-errors)
		     :post (quit-windows-on "*Flycheck errors*")
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
