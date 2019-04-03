(defun my/magithub-pull-request ()
  "Simple pull request command."
  (interactive)
  (with-editor-async-shell-command "hub pull-request"))

(defun my/magithub-browse-default ()
  "Browse the github repo."
  (interactive)
  (magithub--command "browse"))

(defun my/master ()
  "Switch to master and update."
  (interactive)

  (if (magit-changed-files "HEAD")
      (message "Can't switch. You have changes!")
      (magit-checkout "master")
      (magit-pull-from-upstream '()))
  )

(defun my/git-rebase-onto-master ()
  "Rebase the current branch onto origin/master."
  (interactive)
  (accept-process-output
   (magit-fetch-branch "origin" "master" nil)
   5 ;; timeout
   )
  (magit-rebase-branch "origin/master" '("-i" "--autosquash")))

(use-package magit
  :init
  (setq magit-bury-buffer-function 'magit-mode-quit-window
	;; magit-completing-read-function 'magit-ido-completing-read
	magit-log-arguments (quote ("-n20" "--graph" "--decorate"))
	magit-log-select-arguments (quote ("-n20" "--decorate"))
	magit-popup-use-prefix-argument 'default
	magit-save-repository-buffers nil
	magit-display-buffer-function (lambda (buffer)
					(display-buffer
					 buffer (if (and (derived-mode-p 'magit-mode)
							 (memq (with-current-buffer buffer major-mode)
							       '(magit-process-mode
								 magit-revision-mode
								 magit-diff-mode
								 magit-stash-mode
								 magit-status-mode)))
						    nil
						  '(display-buffer-same-window))))
	magit-list-refs-namespaces '("refs/heads" "refs/remotes" "refs/pull"))
  :config
  (setq magit-blame-heading-format "%C | %s")
  :bind (:map base-leader-map
	      ("gs" . magit-status)
	      ("gc" . magit-commit)
	      ("gd" . magit-diff-buffer-file)
	      ("gl" . magit-log-buffer-file)
	      ("gb" . magit-blame)))

(use-package forge
  :after magit)

(use-package github-review)

(use-package magit-org-todos
  :straight (:host github :repo "danielma/magit-org-todos")
  :config
  (magit-org-todos-autoinsert))

(use-package evil-magit
  :after magit
  :init
  (evil-magit-init))

(use-package magithub
  :disabled
  :init
  (setq magithub-features '((pull-request-checkout . t))
	magithub-api-timeout 10)
  :config
  (magithub-feature-autoinject t)
  (magit-define-popup-action 'magithub-dispatch-popup
    ?P "Simple Pull Request" 'my/magithub-pull-request)
  (magit-define-popup-action 'magithub-dispatch-popup
    ?h "Browse Default" 'my/magithub-browse-default))

(use-package browse-at-remote
  :bind (:map base-leader-map
	 ("gB" . browse-at-remote)))

(provide 'dm-magit)
