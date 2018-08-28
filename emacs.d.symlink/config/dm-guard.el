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

(defun rails-test--pluralize (word)
  "Pluralize WORD very stupidly."
  (save-match-data
    (cond
     ((string-match "\\(.+\\)y$" word) (concat (match-string 1 word) "ies"))
     ((string-match "\\(.+\\)tch$" word) (concat (match-string 1 word) "tches"))
     (t (concat word "s")))))

(defvar rails-test-enabled
  t
  "Global variable that can disable rails-tests.")

(defun rails-test-global-toggle ()
  "Globally toggle rails-test runners."
  (interactive)
  (setq rails-test-enabled (not rails-test-enabled))
  (message (if rails-test-enabled "Enabled" "Disabled")))

(defun rails-test ()
  "Use tmux to execute a rails test."
  (interactive)
  (if (and rails-test-enabled projectile-rails-mode)
      (let* ((spec-mode (or (eq (projectile-project-type) 'rails-rspec) (eq (projectile-project-type) 'ruby-rspec)))
             (test-cmd (cond
                        ((eq (projectile-project-type) 'rails-rspec) "bundle exec spring rspec")
                        ((eq (projectile-project-type) 'ruby-rspec) "bundle exec rspec")
                        ((eq (projectile-project-type) 'ruby-test) "bundle exec rake test")
                        (t "bin/rails test")))
             (file-name (file-relative-name buffer-file-name (projectile-project-root)))
             (test-name
              (cond
               ((string-match "_test.rb$" file-name) file-name)
               ((string-match "_spec.rb$" file-name) file-name)
               ((string-match "^app/views" file-name) nil)
               ((string-match "^app/graphs/\\(.+\\)/vertices/\\(.+\\)_vertex.rb$" file-name)
                (let* ((graph-dir (match-string 1 file-name))
                       (vertex-name (match-string 2 file-name))
                       (graph-test-dir (or (and (string-equal graph-dir "app_graph") "") "church_center")))
                  (concat "test/integration/pco/api/" graph-dir "/" (rails-test--pluralize vertex-name) "_test.rb")))
               ((string-match "^app/\\(.+\\).rb$" file-name)
                (if spec-mode
                    (concat "spec/" (match-string 1 file-name) "_spec.rb")
                  (concat "test/" (match-string 1 file-name) "_test.rb")))
               ((string-match "^lib/\\(.+\\).rb$" file-name)
                (if spec-mode
                    (concat "spec/" (match-string 1 file-name) "_spec.rb")
                  (concat "test/" (match-string 1 file-name) "_test.rb")))
               ((string-match "^test/fixtures/\\(.+\\).yml$" file-name)
                (concat "test/models/" (rails-test--singularize (match-string 1 file-name)) "_test.rb"))
               (t nil))))
        (if test-name
            (emamux:send-command (concat "cd " (projectile-project-root) " && " test-cmd " " test-name))))))

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
