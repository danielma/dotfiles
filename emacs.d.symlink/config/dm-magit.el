;;; dm-magit.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun my/commit-mode-setup ()
  "Set commit mode."
  (setq-local fill-column 70))

(defun my/with-editor-commit-mode-setup ()
  "Use `git-commit-mode' for with-editor commit buffers."
  (when with-editor-mode
    (git-commit-mode)))

(use-package magit
  :defer t
  :custom
  (git-commit-major-mode 'markdown-mode)
  (magit-list-refs-sortby "-committerdate")
  (magit-define-global-key-bindings 'recommended)
  (magit-repository-directories '(("~/Code" . 0)))
  :hook
  (magit-status-mode . (lambda () (meow-mode -1)))
  (with-editor-mode . my/with-editor-commit-mode-setup)
  (git-commit-setup . my/commit-mode-setup)
  :bind (:map magit-status-mode-map ("SPC" . meow-keypad)))

(use-package forge
  :after magit
  :custom
  (forge-list-buffer-default-topic-filters
   (forge--topics-spec :type 'topic :active nil :state 'open :status 'inbox :order 'newest))
  (forge-status-buffer-default-topic-filters
   (forge--topics-spec :type 'topic :active nil :state 'open :status 'inbox :order 'newest)))

(use-package browse-at-remote
  :after magit
  :custom
  (browse-at-remote-prefer-symbolic nil)
  :config
  (advice-add 'browse-at-remote-kill :around #'dm-with-select-clipboard))

(defun my/pr ()
  "Simple pull request command."
  (interactive)
  (with-editor-async-shell-command "gh-pr-create-from-log"))

(defun my/main ()
  "Switch to main and update."
  (interactive)
  
  (if (magit-changed-files "HEAD")
      (message "Can't switch. You have changes!")
    (magit-process-git nil "checkout" "main")
    (magit-pull-from-upstream '())))

(defun my/git-rebase-onto-main ()
  "Rebase the current branch onto origin/main."
  (interactive)
  (magit-process-git nil "fetch" "origin" "main")
  (magit-rebase-branch "origin/main" '("-i" "--autosquash")))

(provide 'dm-magit)
