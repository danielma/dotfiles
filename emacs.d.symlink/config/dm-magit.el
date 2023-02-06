;;; dm-magit.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my/commit-mode-setup ()
  "Set commit mode."
  (setq-local fill-column 70))

(use-package magit
  :custom
  (git-commit-major-mode 'markdown-mode)
  :bind (("C-c g" . magit-file-dispatch))
  :hook
  (git-commit-setup . my/commit-mode-setup))

(use-package forge
  :after magit)

(use-package browse-at-remote
  :after magit
  :config
  (advice-add 'browse-at-remote-kill :around #'with-select-clipboard))

(defun my/magithub-pull-request ()
  "Simple pull request command."
  (interactive)
  (with-editor-async-shell-command "hub pull-request "))

(defun my/main ()
  "Switch to main and update."
  (interactive)
  
  (if (magit-changed-files "HEAD")
      (message "Can't switch. You have changes!")
    (magit-checkout "main")
    (magit-pull-from-upstream '())))

(if t
    t


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
     10 ;; timeout
     )
    (magit-rebase-branch "origin/master" '("-i" "--autosquash")))

  (defun my/git-rebase-onto-main ()
    "Rebase the current branch onto origin/main."
    (interactive)
    (accept-process-output
     (magit-fetch-branch "origin" "main" nil)
     10 ;; timeout
     )
    (magit-rebase-branch "origin/main" '("-i" "--autosquash")))

  (use-package magit
    :custom
    (magit-bury-buffer-function 'magit-mode-quit-window)
    (magit-popup-use-prefix-argument 'default)
    (magit-save-repository-buffers nil)
    ;; (magit-display-buffer-function (lambda (buffer)
    ;;       			   (if (equal (buffer-name) "*scratch*")
    ;;       			       (display-buffer buffer '(display-buffer-same-window))
    ;;       			     (magit-display-buffer-traditional buffer))))
    (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
    (git-commit-major-mode 'markdown-mode)
    :config
    (bind-key "<SPC>" base-leader-map magit-mode-map)
    :hook
    (git-commit-setup . my/commit-mode-setup)
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

  (use-package browse-at-remote
    :bind (:map base-leader-map
	              ("gB" . browse-at-remote)))

  (defun browse-at-remote-pbcopy ()
    (interactive)
    (paste-to-osx (browse-at-remote-get-url)))
  )

(provide 'dm-magit)
