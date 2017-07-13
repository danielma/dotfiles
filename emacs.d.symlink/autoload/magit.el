(require 'magit)
(require 'magithub)
(require 'evil-magit)

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
  (magithub--command-with-editor "pull-request"))

(defun my/magithub-browse-default ()
  "Browse the github repo."
  (interactive)
  (magithub--command "browse"))

(defun my/git-rebase-onto-master ()
  "Rebase the current branch onto origin/master."
  (interactive)
  (accept-process-output
   (magit-fetch-branch "origin" "master" nil)
   5 ;; timeout
   )
  (magit-rebase "origin/master" '("-i" "--autosquash")))

(magit-define-popup-action 'magithub-dispatch-popup
  ?P "Simple Pull Request" 'my/magithub-pull-request)

(magit-define-popup-action 'magithub-dispatch-popup
  ?h "Browse Default" 'my/magithub-browse-default)

(setq magithub-features '((pull-request-checkout . t)))
(magithub-feature-autoinject 'all)
