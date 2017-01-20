(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))

(defun my/magithub-pull-request ()
  "Simple pull request command."
  (interactive)
  (magithub--command-with-editor "pull-request" (magithub-pull-request-arguments)))

(defun my/magithub-browse-default ()
  "Browse the github repo."
  (interactive)
  (magithub--command "browse"))

(defun my/git-rebase-onto-master ()
  "Rebase the current branch onto origin/master."
  (interactive)
  (magit-fetch-branch "origin" "master" nil)
  (magit-rebase "origin/master" "-i"))

(magit-define-popup-action 'magithub-pull-request-popup
  ?p "Simple Pull Request" 'my/magithub-pull-request)

(magit-define-popup-action 'magithub-dispatch-popup
  ?h "Browse Default" 'my/magithub-browse-default)
