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

(defvar dm-guard-only-failures
  nil
  "Global variable to run only failres.")

(defvar dm-guard-line-mode
  nil
  "Global variable to run only the current test.")

(defun dm-guard-global-toggle ()
  "Globally toggle dm-guard runners."
  (interactive)
  (setq dm-guard-enabled (not dm-guard-enabled))
  (message (if dm-guard-enabled "Enabled" "Disabled")))

(defun dm-guard-line-mode ()
  "Globally toggle line mode for runner."
  (interactive)
  (setq dm-guard-line-mode (not dm-guard-line-mode))
  (message (if dm-guard-line-mode "Enabled" "Disabled")))

(defun dm-guard-toggle-only-failures ()
  "Globally toggle line mode for runner."
  (interactive)
  (setq dm-guard-only-failures (not dm-guard-only-failures))
  (message (if dm-guard-only-failures "Enabled" "Disabled")))

(defun dm-guard-test ()
  "Use tmux to execute a rails test."
  (interactive)
  (if (and dm-guard-enabled (projectile-project-p))
      (if (and dm-guard-line-mode (--dm-guard-is-test-file-p)) (dm-guard-test-line) (dm-guard-test-file))))

(defun dm-guard-test-file ()
  "Use tmux to execute a test for the current file."
  (interactive)
  (let* ((test-cmd (--dm-guard-test-command))
         (test-name (--dm-guard-test-name)))
    (if test-name
        (--dm-guard-clear-and-run (concat test-cmd " " test-name)))))

(defun dm-guard-test-line ()
  "Use tmux to execute the test on the current line."
  (interactive)
  (let* ((test-cmd (--dm-guard-test-command))
         (test-name (--dm-guard-test-name))
         (current-line (format-mode-line "%l")))
    (if test-name
        (--dm-guard-clear-and-run (concat test-cmd " " test-name ":" current-line)))))

(defun --dm-guard-clear-and-run (command)
  "Use tmux to clear and execute COMMAND."
  (if command
      (let ((full-command (concat "cd " (projectile-project-root) " && " command)))
        (emamux:send-command (concat "clear; echo -e '" command "'; " full-command)))))

(defun --dm-guard-rspec-test-command ()
  "Generate the test command for rspec."
  (let ((base "rspec --format=documentation --color"))
    (if dm-guard-only-failures
        (concat base " --only-failures")
      base)))

(defun --dm-guard-test-command ()
  "Generate the test command for a buffer."
  (let* ((project-type (projectile-project-type))
         (file-path (buffer-file-name dm-guard-manual-test-buffer))
         (file-name (file-relative-name file-path (projectile-project-root)))
         (test-cmd (cond
                    ((string-match ".js$" file-name) "yarn run test --colors")
                    ((string-match ".ts$" file-name) "yarn run test --colors")
                    ((eq project-type 'rails-rspec) (concat "bundle exec spring " (--dm-guard-rspec-test-command)))
                    ((eq project-type 'ruby-rspec) (concat "bundle exec " (--dm-guard-rspec-test-command)))
                    ((eq project-type 'rubygem) (concat "bundle exec " (--dm-guard-rspec-test-command)))
                    ((eq project-type 'rails-test) "bin/rails test")
                    ((eq project-type 'ruby-test) "bundle exec ruby")
                    (t "ruby"))))
    test-cmd))

(defun --dm-guard-is-test-file-p (&optional buffer)
  "Is the BUFFER a test or implementation?."
  (let* ((file-path (buffer-file-name buffer)))
    (cond
     ((string-match "\\(_\\|.\\)test.\\(rb\\|js\\|ts\\)$" file-path) t)
     ((string-match "_spec.rb$" file-path) t)
     (t nil))))

(defun --dm-guard-test-name ()
  "Generate the test name for a buffer."
  (let* ((project-type (projectile-project-type))
         (spec-mode (or (eq project-type 'rails-rspec) (eq project-type 'ruby-rspec)))
         (is-test-file (--dm-guard-is-test-file-p dm-guard-manual-test-buffer))
         (file-path (buffer-file-name dm-guard-manual-test-buffer))
         (file-name (file-relative-name file-path (projectile-project-root)))
         (test-path
          (cond
           (is-test-file file-name)
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
           (t nil))))
    ;; (cond ((and test-path (eq project-type 'ruby-test)) (concat "TEST=" test-path))
    ;;       (t test-path))))
    test-path))

(defun dm-guard-select-test-buffer (buffer)
  "Select a BUFFER to use as the test file."
  (interactive "b")
  (setq-local dm-guard-manual-test-buffer (get-buffer buffer)))

(defun dm-guard-clear-test-buffer ()
  "Remove custom buffer for testing."
  (interactive)
  (setq-local dm-guard-manual-test-buffer nil))

(define-minor-mode dm-guard-mode
  "Use emamux to test after saving a file."
  :init-value nil
  :lighter " dm-guard"
  (cond ((bound-and-true-p dm-guard-mode)
         (add-hook 'after-save-hook #'dm-guard-test t t))
        (t
         (remove-hook 'after-save-hook #'dm-guard-test t))))

(use-package emamux
  :bind (:map base-leader-map
              ("kl" . dm-guard-test-line)))

(provide 'dm-guard)
;;; dm-guard.el ends here
