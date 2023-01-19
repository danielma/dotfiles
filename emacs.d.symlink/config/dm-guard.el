;;; dm-guard --- Does the job of Guard, but uses emacs file save hooks instead of watching -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun dm-guard--singularize (word)
  "Singularize WORD very stupidly."
  (save-match-data
    (cond
     ((equal "people" word) "person")
     ((string-match "\\(.+\\)ies$" word) (concat (match-string 1 word) "y"))
     ((string-match "\\(.+\\)s$" word) (match-string 1 word))
     (t (error (concat "Can't singularize " word))))))

(defun dm-guard--pluralize (word)
  "Pluralize WORD very stupidly."
  (save-match-data
    (cond
     ((equal "person" word) "people")
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

(defvar dm-guard-terminal
  'async-shell
  "Which terminal to use for running tests.")

(defvar dm-guard-async-shell-buffer
  nil
  "Side buffer for running tests.")

(defvar dm-guard-async-shell-process
  nil
  "Process for running tests.")

(defvar dm-guard--currently-running-test
  nil
  "Currently running test.")

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
  (if (and dm-guard-enabled (project-current))
      (if (and dm-guard-line-mode (--dm-guard-is-test-file-p)) (dm-guard-test-line) (dm-guard-test-file))))

(defun dm-guard-test-file ()
  "Use tmux to execute a test for the current file."
  (interactive)
  (let* ((test-cmd (--dm-guard-test-command))
         (test-name (--dm-guard-test-name)))
    (if test-name
        (--dm-guard-clear-and-run test-cmd test-name))))

(defun dm-guard-test-line ()
  "Use tmux to execute the test on the current line."
  (interactive)
  (let* ((test-cmd (--dm-guard-test-command))
         (test-name (--dm-guard-test-name))
         (current-line (format-mode-line "%l")))
    (if test-name
        (--dm-guard-clear-and-run test-cmd test-name current-line))))

(defun --dm-guard-ensure-async-shell-buffer ()
  "Make sure the test buffer exists."
  (or (and (buffer-live-p dm-guard-async-shell-buffer) dm-guard-async-shell-buffer)
      (let ((buffer (generate-new-buffer "*Guard Process*")))
        (setq dm-guard-async-shell-buffer buffer)
        (with-current-buffer buffer
          (shell-mode)
          (setq-local process-environment (append (comint-term-environment) process-environment)))
        (--dm-guard-ensure-async-shell-buffer))))

(defun --dm-guard-async-shell-process-sentinel (proc msg)
  "Sentinel for async shell process. Takes PROC and MSG and responds."
  (if (memq (process-status proc) '(exit signal))
      (setq dm-guard--currently-running-test nil)))

(defun --dm-guard-clear-and-run (test-cmd test-name &optional current-line)
  "Use TEST-CMD to test TEST-NAME, and optionally only the CURRENT-LINE."
  (let* ((file-command (concat test-cmd " " test-name))
         (line-command (if current-line (concat file-command ":" current-line) file-command))
         (full-command (concat "cd " (project-root (project-current)) " && " line-command)))
    (pcase dm-guard-terminal
      ('emamux
       (emamux:send-keys "^c")
       (emamux:send-command (concat " clear; echo -e '" command "'; " full-command)))
      ;; this should probably just be compile mode
      ('async-shell
       (if (string-equal dm-guard--currently-running-test line-command)
           'already-running
         (setq dm-guard--currently-running-test line-command)
         (let ((buf (--dm-guard-ensure-async-shell-buffer)))
           (display-buffer-in-side-window buf '((side . right) (window-width . 0.2)))
           (with-current-buffer buf
             (if dm-guard-async-shell-process (delete-process dm-guard-async-shell-process))
             (erase-buffer)
             (insert line-command "\n\n")
             (setq dm-guard-async-shell-process
                   (make-process
                    :name "Guard tests"
                    :buffer buf
                    :command (list shell-file-name shell-command-switch full-command)
                    :filter 'comint-output-filter
                    :sentinel #'--dm-guard-async-shell-process-sentinel))))))
      (_ (error (format "Unusable terminal `%s'" dm-guard-terminal))))))

(defun --dm-guard-rspec-test-command ()
  "Generate the test command for rspec."
  (let ((base "rspec --format=documentation --color"))
    (if dm-guard-only-failures
        (concat base " --only-failures")
      base)))


(defun --dm-guard-test-command ()
  "Generate the test command for a buffer."
  (let* ((project-type (project-type))
         (file-path (buffer-file-name dm-guard-manual-test-buffer))
         (file-name (file-relative-name file-path (project-root (project-current))))
         (test-cmd (cond
                    ((string-match ".js$" file-name) "yarn run test --colors")
                    ((string-match ".ts$" file-name) "yarn run test --colors")
                    ((eq project-type 'rails-rspec) (concat "bin/spring " (--dm-guard-rspec-test-command)))
                    ((eq project-type 'ruby-rspec) (concat "bundle exec " (--dm-guard-rspec-test-command)))
                    ((eq project-type 'rubygem) (concat "bundle exec " (--dm-guard-rspec-test-command)))
                    ((eq project-type 'rails-test) "bin/rails test")
                    ((eq project-type 'ruby-test) "bundle exec ruby")
                    ((eq project-type 'swift-package) "swift test")
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
  (let* ((project-type (project-type))
         (spec-mode (project-verify-file "spec"))
         (is-test-file (--dm-guard-is-test-file-p dm-guard-manual-test-buffer))
         (file-path (buffer-file-name dm-guard-manual-test-buffer))
         (file-name (file-relative-name file-path (project-root (project-current)))))
    (cond
     ((eq project-type 'swift-package) "")
     (t (cond
         (is-test-file file-name)
         ((string-match "^app/views" file-name) nil)
         ((string-match "^app/graphs/\\(.+\\)/vertices/\\(.+\\)_vertex.rb$" file-name)
          (let* ((graph-dir (match-string 1 file-name))
                 (vertex-name (match-string 2 file-name))
                 (graph-test-dir (or (and (string-equal graph-dir "app_graph") "") "church_center")))
            (concat "test/integration/pco/api/" graph-dir "/" (dm-guard--pluralize vertex-name) "_test.rb")))
         ((string-match "^app/graphs/\\(.+\\).rb$" file-name)
          (if spec-mode
              (concat "spec/requests/graphs/" (match-string 1 file-name) "_spec.rb")
            (concat "test/integration" (match-string 1 file-name) "_test.rb")))
         ((string-match "^app/\\(.+\\).rb$" file-name)
          (if spec-mode
              (concat "spec/" (match-string 1 file-name) "_spec.rb")
            (concat "test/" (match-string 1 file-name) "_test.rb")))
         ((string-match "^lib/\\(.+\\).rb$" file-name)
          (if spec-mode
              (concat "spec/" (match-string 1 file-name) "_spec.rb")
            (concat "test/" (match-string 1 file-name) "_test.rb")))
         ((string-match "^test/fixtures/\\(.+\\).yml$" file-name)
          (concat "test/models/" (dm-guard--singularize (match-string 1 file-name)) "_test.rb"))
         (t nil))))))

(defun dm-guard-select-test-buffer (buffer)
  "Select a BUFFER to use as the test file."
  (interactive "b")
  (setq-local dm-guard-manual-test-buffer (get-buffer buffer)))

(defun dm-guard-clear-test-buffer ()
  "Remove custom buffer for testing."
  (interactive)
  (setq-local dm-guard-manual-test-buffer nil))

(defvar dm-guard-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" 'dm-guard-test-line)
    (define-key map "t" 'dm-guard-test)
    (define-key map "b" 'dm-guard-select-test-buffer)
    map)
  "The global map for dm-guard.")

(defvar dm-guard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-ck" dm-guard-map)
    map)
  "Keymap of dm-guard.")

(define-minor-mode dm-guard-mode
  "Use emamux to test after saving a file."
  :init-value nil
  :lighter " guard"
  :map dm-guard-mode-map
  (cond ((bound-and-true-p dm-guard-mode)
         (add-hook 'after-save-hook #'dm-guard-test t t))
        (t
         (remove-hook 'after-save-hook #'dm-guard-test t))))

(use-package emamux)

(provide 'dm-guard)
;;; dm-guard.el ends here
