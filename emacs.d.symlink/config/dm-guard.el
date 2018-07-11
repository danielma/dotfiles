;;; dm-guard --- Does the job of Guard, but uses emacs file save hooks instead of watching

;;; Commentary:

;;; Code:

(defun rails-test--singularize (word)
  "Singularize WORD very stupidly."
  (save-match-data
    (cond
     ((string-match "\\(.+\\)ies$" word) (concat (match-string 1 word) "y"))
     ((string-match "\\(.+\\)s$" word) (match-string 1 word))
     (t (error (concat "Can't singularize " word))))))

(defun rails-test ()
  "Use tmux to execute a rails test."
  (interactive)
  (if projectile-rails-mode
      (let* ((file-name (file-relative-name buffer-file-name (projectile-project-root)))
             (test-name
              (cond
               ((string-match "_test.rb$" file-name) file-name)
               ((string-match "^app/views" file-name) nil)
               ((string-match "^app/\\(.+\\).rb$" file-name)
                (concat "test/" (match-string 1 file-name) "_test.rb"))
               ((string-match "^test/fixtures/\\(.+\\).yml$" file-name)
                (concat "test/models/" (rails-test--singularize (match-string 1 file-name)) "_test.rb"))
               ((string-match ".rb$" file-name) file-name)
               (t nil))))
        (if test-name
            (emamux:send-command (concat "cd " (projectile-project-root) " && bin/rails test " test-name))))))

(define-minor-mode rails-test-mode
  "Use emamux to test after saving a file."
  :init-value nil
  :lighter " rails-test"
  (cond ((bound-and-true-p rails-test-mode)
         (add-hook 'after-save-hook #'rails-test t t))
        (t
         (remove-hook 'after-save-hook #'rails-test t))))

(use-package emamux)

(provide 'dm-guard)
;;; dm-guard.el ends here
