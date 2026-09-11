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
  :commands (magit-get-current-branch magit-toplevel)
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

(defvar dm-magit-remote-repositories nil
  "Remote repositories cloned for exploration during this Emacs session.")

(defun dm-magit--remote-url (repository)
  "Return a clone URL for REPOSITORY.

REPOSITORY may be a URL or a GitHub OWNER/NAME reference."
  (if (string-match-p "\\`\\(?:https?://\\|ssh://\\|git@\\)" repository)
      repository
    (format "git@github.com:%s.git"
            (replace-regexp-in-string "\\.git\\'" "" repository))))

(defun dm-magit--temporary-repository-directory (repository)
  "Create a temporary directory named after REPOSITORY."
  (let* ((escaped (replace-regexp-in-string "[^[:alnum:]]+" "_" repository))
         (short-name (substring escaped 0 (min 40 (length escaped)))))
    (make-temp-file (format "dm-magit-%s-" short-name) t)))

(defun dm-magit-explore-remote (repository)
  "Clone and visit a remote REPOSITORY for temporary exploration.

Reuse a successful clone when REPOSITORY was already explored during this
Emacs session.  Input without a URL scheme is treated as a GitHub OWNER/NAME
reference."
  (interactive
   (list (completing-read "Repository URL or GitHub reference: "
                          dm-magit-remote-repositories)))
  (require 'magit)
  (let* ((repository-url (dm-magit--remote-url repository))
         (existing (assoc-string repository-url dm-magit-remote-repositories)))
    (if (and existing
             (file-directory-p (expand-file-name ".git" (cdr existing))))
        (magit-status (cdr existing))
      (when existing
        (setq dm-magit-remote-repositories
              (delete existing dm-magit-remote-repositories)))
      (unless (magit-git-success "ls-remote" "--exit-code" repository-url)
        (user-error "Could not find repository at %s" repository-url))
      (let ((directory (dm-magit--temporary-repository-directory repository-url))
            (magit-clone-set-remote.pushDefault t))
        (condition-case error-data
            (progn
              (push (cons repository-url directory) dm-magit-remote-repositories)
              (magit-clone-regular repository-url directory nil))
          (error
           (setq dm-magit-remote-repositories
                 (assoc-delete-all repository-url dm-magit-remote-repositories))
           (delete-directory directory t)
           (signal (car error-data) (cdr error-data))))))))

(defalias 'remote-repository-explore #'dm-magit-explore-remote)

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
