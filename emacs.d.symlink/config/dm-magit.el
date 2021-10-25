(defun my/magithub-pull-request ()
  "Simple pull request command."
  (interactive)
  (with-editor-async-shell-command "hub pull-request"))

;; (defun my/magithub-browse-default ()
;;   "Browse the github repo."
;;   (interactive)
;;   (magithub--command "browse"))

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
  :custom
  (magit-bury-buffer-function 'magit-mode-quit-window)
  (magit-popup-use-prefix-argument 'default)
  (magit-save-repository-buffers nil)
  (magit-display-buffer-function (lambda (buffer)
				   (if (equal (buffer-name) "*scratch*")
				       (display-buffer buffer '(display-buffer-same-window))
				     (magit-display-buffer-traditional buffer))))
  :config
  (bind-key "<SPC>" base-leader-map magit-mode-map)
  :bind
  (:map base-leader-map
	("gs" . magit-status)
	("gc" . magit-commit)
	("gd" . magit-diff-buffer-file)
	("gl" . magit-log-buffer-file)
	("gb" . magit-blame)))

(use-package forge
  :after magit)

(use-package github-review)

(use-package dm-magit-markdown-todos
  :straight nil
  :config
  (magit-markdown-todos-autoinsert))

(use-package evil-collection
  :after (magit evil)
  :init
  (evil-collection-init))

(use-package browse-at-remote
  :bind (:map base-leader-map
	 ("gB" . browse-at-remote)))

(provide 'dm-magit)
