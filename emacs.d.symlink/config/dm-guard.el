(defun rails-test ()
  "Use tmux to execute a rails test."
  (interactive)
  (let* ((file-name (file-relative-name buffer-file-name (projectile-project-root)))
         (test-name
          (cond
           ((string-match "_test.rb$" file-name) file-name)
           ((string-match "^app/\\(.+\\).rb$" file-name) (concat "test/" (match-string 1 file-name) "_test.rb"))
           (t file-name))))
    (emamux:send-command (concat "bin/rails test " test-name))))

(define-minor-mode rails-test-mode
  "Use emamux to test after saving a file."
  :init-value nil
  :lighter " rails-test"
  (if rails-test-mode
      (add-hook 'after-save-hook #'rails-test t t)
    (remove-hook 'after-save-hook #'rails-test t)))

(use-package emamux)

(provide 'dm-guard)
