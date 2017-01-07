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

(magit-define-popup-action 'magithub-pull-request-popup
  ?p "Simple Pull Request" 'my/magithub-pull-request)
