;;; dm-flycheck.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(transient-define-prefix flymake-transient-menu ()
  [:description "Commands"
                (3 "l" "List" consult-flymake)
                (3 "n" "Next" flymake-goto-next-error)
                (3 "p" "Prev" flymake-goto-prev-error)])

(defun ruby-flymake-rubocop--with-gemfile-lock (orig-fun &rest args)
  "Wrap around ORIG-FUN with ARGS to check Gemfile.lock for Rubocop."
  (let* ((dir (car args))
         (lock (expand-file-name "Gemfile.lock" dir)))
    (or
     (and (file-exists-p lock)
          (with-temp-buffer
            (insert-file-contents lock)
            (re-search-forward "^ *rubocop ([[:digit:]]" nil t)))
     (apply orig-fun args))))

(use-package flymake
  :delight
  :bind (("C-c ." . flymake-transient-menu))
  :config
  (advice-add 'ruby-flymake-rubocop--use-bundler-p :around 'ruby-flymake-rubocop--with-gemfile-lock)
  :custom
  (flymake-show-diagnostics-at-end-of-line nil)
  )

(use-package flymake-posframe
  :straight (:type git :host github :repo "Ladicle/flymake-posframe")
  :hook (flymake-mode . flymake-posframe-mode))

(defun my/use-rubocop-from-bundle ()
  (let ((root (locate-dominating-file
	             (or (buffer-file-name) default-directory)
	             "Gemfile")))
    (when root
      (setq-local flycheck-command-wrapper-function
		              (lambda (command)
		                (append '("bundle" "exec") command))))))

(use-package flycheck
  :disabled
  :after delight
  :config
  (global-flycheck-mode)
  :delight
  :custom
  (flycheck-ruby-rubocop-executable "rubocop")
  (flycheck-disabled-checkers '(ruby-reek))
  :bind (:map flycheck-command-map
              ("l" . consult-flycheck)
              ("L" . flycheck-list-errors))
  :hook
  (flycheck-mode . my/use-eslint-from-node-modules)
  (flycheck-mode . my/use-rubocop-from-bundle)
  :config
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode))

(use-package flycheck-inline
  :disabled
  :after flycheck
  :config
  (global-flycheck-inline-mode))

(use-package flycheck-posframe
  :after flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  :init
  (flycheck-posframe-configure-pretty-defaults))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  ;; (setq-local flycheck-javascript-eslint-executable "eslint_d"))
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))


(if t
    t

  ;; todo: this should open in bottom 20%
  (defun custom-flycheck-toggle-errors ()
    (interactive)
    (if (get-buffer "*Flycheck errors*")
        (progn
          (delete-window (get-buffer-window (get-buffer "*Flycheck errors*")))
          (kill-buffer "*Flycheck errors*"))
      (flycheck-list-errors)))

  (defun my/use-rubocop-from-bin ()
    (let ((root (locate-dominating-file
                 (or (buffer-file-name) default-directory)
                 "Gemfile")))
      (when root
        (setq-local flycheck-command-wrapper-function
                    (lambda (command)
                      (append '("bundle" "exec") command))))))

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
  )


(provide 'dm-flycheck)

;;; dm-flycheck.el ends here
