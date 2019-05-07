;;; dm-guard --- Does the job of Guard, but uses emacs file save hooks instead of watching

;;; Commentary:

;;; Code:

(defun dm-guard--singularize (word)
  "Singularize WORD very stupidly."
  (save-match-data
    (cond
     ((string-match "\\(.+\\)ies$" word) (concat (match-string 1 word) "y"))
     ((string-match "\\(.+\\)s$" word) (match-string 1 word))
     (t (error (concat "Can't singularize " word))))))

(defun dm-guard--pluralize (word)
  "Pluralize WORD very stupidly."
  (save-match-data
    (cond
     ((string-match "\\(.+\\)y$" word) (concat (match-string 1 word) "ies"))
     ((string-match "\\(.+\\)tch$" word) (concat (match-string 1 word) "tches"))
     (t (concat word "s")))))

(defvar dm-guard-manual-test-buffer
  nil
  "Manually set the test buffer if guard patterns are insufficient.")
(make-local-variable 'dm-guard-manual-test-buffer)

(defvar dm-guard-known-project-types
  '(rails-rspec ruby-rspec rails-test ruby-test)
  "Known project types for dm-guard.")

(defvar dm-guard-enabled
  t
  "Global variable that can disable dm-guard.")

(defun dm-guard-global-toggle ()
  "Globally toggle dm-guard runners."
  (interactive)
  (setq dm-guard-enabled (not dm-guard-enabled))
  (message (if dm-guard-enabled "Enabled" "Disabled")))

(defun dm-guard-test ()
  "Use tmux to execute a rails test."
  (interactive)
  (if (and dm-guard-enabled (projectile-project-p))
      (let* ((project-type (projectile-project-type))
             (spec-mode (or (eq project-type 'rails-rspec) (eq project-type 'ruby-rspec)))
             (file-path (buffer-file-name dm-guard-manual-test-buffer))
             (file-name (file-relative-name file-path (projectile-project-root)))
             (test-cmd (cond
                        ((string-match ".js$" file-name) "yarn run test-base-command --colors")
                        ((eq project-type 'rails-rspec) "bundle exec spring rspec")
                        ((eq project-type 'ruby-rspec) "bundle exec rspec --color")
                        ((eq project-type 'rails-test) "bin/rails test")
                        ((eq project-type 'ruby-test) "ruby")
                        (t "ruby")))
             (test-path
              (cond
               ((string-match "_test.\\(rb\\|js\\)$" file-name) file-name)
               ((string-match "_spec.rb$" file-name) file-name)
               ((string-match "^app/views" file-name) nil)
               ((string-match "^app/graphs/\\(.+\\)/vertices/\\(.+\\)_vertex.rb$" file-name)
                (let* ((graph-dir (match-string 1 file-name))
                       (vertex-name (match-string 2 file-name))
                       (graph-test-dir (or (and (string-equal graph-dir "app_graph") "") "church_center")))
                  (concat "test/integration/pco/api/" graph-dir "/" (dm-guard--pluralize vertex-name) "_test.rb")))
               ((string-match "^app/\\(.+\\).rb$" file-name)
                (if spec-mode
                    (concat "spec/" (match-string 1 file-name) "_spec.rb")
                  (concat "test/" (match-string 1 file-name) "_test.rb")))
               ((string-match "^lib/\\(.+\\).rb$" file-name)
                (if spec-mode
                    (concat "spec/lib/" (match-string 1 file-name) "_spec.rb")
                  (concat "test/lib/" (match-string 1 file-name) "_test.rb")))
               ((string-match "^test/fixtures/\\(.+\\).yml$" file-name)
                (concat "test/models/" (dm-guard--singularize (match-string 1 file-name)) "_test.rb"))
               (t nil)))
             (test-name
              ;; (cond ((and test-path (eq project-type 'ruby-test)) (concat "TEST=" test-path))
              ;;       (t test-path))))
              test-path))
        (if test-name
            (let* ((command (concat test-cmd " " test-name))
                   (full-command (concat "cd " (projectile-project-root) " && " command)))
              (emamux:send-command (concat "clear; echo -e '" command "'; " full-command)))))))

(defun dm-guard-select-test-buffer (buffer)
  "Select a BUFFER to use as the test file."
  (interactive "b")
  (setq-local dm-guard-manual-test-buffer (get-buffer buffer)))

(define-minor-mode dm-guard-mode
  "Use emamux to test after saving a file."
  :init-value nil
  :lighter " dm-guard"
  (cond ((bound-and-true-p dm-guard-mode)
         (add-hook 'after-save-hook #'dm-guard-test t t))
        (t
         (remove-hook 'after-save-hook #'dm-guard-test t))))

(use-package emamux)

(provide 'dm-guard)
;;; dm-guard.el ends here
