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
  (magit-rebase "origin/master" '("-i" "--autosquash")))

(defun my/magit-get-todo-file ()
  (let* ((toplevel (magit-toplevel))
         (todo (concat toplevel "todo.org")))
    todo))

(defun my/magit-insert-org-todo ()
  "Insert org todos from the local ~/todo.org."
  (when (file-readable-p (my/magit-get-todo-file))
    (let ((todos (with-temp-buffer
                      (insert-file-contents (my/magit-get-todo-file))
                      (org-mode)
                      (org-element-map (org-element-parse-buffer) 'headline
                        (lambda (headline)
                          (let ((todo-keyword (org-element-property :todo-keyword headline))
                                (todo-type (org-element-property :todo-type headline))
                                (title (org-element-property :raw-value headline)))
                            (and (eq todo-type 'todo)
                                 (concat "* " (propertize todo-keyword 'face 'org-todo) " " title))))))))
      (magit-insert-section (org-todo)
        (magit-insert-heading "Todos:")
        (--map
         (progn
           (magit-insert-section (org-todo)
             (insert it)
             (insert ?\n)))
         todos)
      (insert ?\n)))))

(defun my/magit-visit-org-todo ()
  "Visits the org todo file."
  (interactive)
  (find-file (my/magit-get-todo-file)))

(defvar magit-org-todo-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m [remap magit-visit-thing] 'my/magit-visit-org-todo)
    m))

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
  (evil-magit-init)
  (setq magit-blame-heading-format "%C | %s")
  (magit-add-section-hook 'magit-status-sections-hook 'my/magit-insert-org-todo 'magit-insert-staged-changes t)
  :bind (:map base-leader-map
	      ("gs" . magit-status)
	      ("gc" . magit-commit)
	      ("gd" . magit-diff-buffer-file)
	      ("gl" . magit-log-buffer-file)
	      ("gb" . magit-blame)))

(use-package evil-magit
  :commands (evil-magit-init))

(use-package magithub
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
