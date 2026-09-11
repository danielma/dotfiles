;;; dm-guard --- Does the job of Guard, but uses emacs file save hooks instead of watching -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'dm-projects)

(declare-function ghostel-compile "ghostel-compile")
(defvar compilation-always-kill)
(defvar ghostel-compile-buffer-name)

(defun dm-guard--singularize (word)
  "Singularize WORD very stupidly."
  (save-match-data
    (cond
     ((equal "people" word) "person")
     ((string-match "\\(.+\\)ies$" word) (concat (match-string 1 word) "y"))
     ((string-match "\\(.+\\)s$" word) (match-string 1 word))
     (t (error "Can't singularize %s" word)))))

(defun dm-guard--pluralize (word)
  "Pluralize WORD very stupidly."
  (save-match-data
    (cond
     ((equal "person" word) "people")
     ((string-match "\\(.+\\)y$" word) (concat (match-string 1 word) "ies"))
     ((string-match "\\(.+\\)tch$" word) (concat (match-string 1 word) "tches"))
     (t (concat word "s")))))

(defvar-local dm-guard-manual-test-buffer nil
  "Buffer to test when the automatic test mapping is insufficient.")

(defvar dm-guard-enabled t
  "Whether Guard test runners may run.")

(defvar dm-guard-line-mode-enabled nil
  "Whether saving a test buffer runs only the test at point.")

(defvar dm-guard-only-failures nil
  "Whether RSpec should run only previously failing examples.")

(defvar dm-guard-buffer-name "*Guard Process*"
  "Name of the Ghostel buffer used to render test output.")

(defvar dm-guard--currently-running-test nil
  "Command for the currently running test.")

(defun dm-guard-global-toggle ()
  "Toggle all Guard test runners."
  (interactive)
  (setq dm-guard-enabled (not dm-guard-enabled))
  (message "%s Guard test runners" (if dm-guard-enabled "Enabled" "Disabled")))

(defun dm-guard-toggle-line-mode ()
  "Toggle running only the test at point when saving test buffers."
  (interactive)
  (setq dm-guard-line-mode-enabled (not dm-guard-line-mode-enabled))
  (message "%s Guard line mode" (if dm-guard-line-mode-enabled "Enabled" "Disabled")))

(defun dm-guard-toggle-only-failures ()
  "Toggle running only previously failing RSpec examples."
  (interactive)
  (setq dm-guard-only-failures (not dm-guard-only-failures))
  (message "%s RSpec failures-only mode" (if dm-guard-only-failures "Enabled" "Disabled")))

(defun dm-guard--selected-buffer ()
  "Return the manually selected test buffer or the current buffer."
  (if (buffer-live-p dm-guard-manual-test-buffer)
      dm-guard-manual-test-buffer
    (current-buffer)))

(defun dm-guard--relative-file-name ()
  "Return the selected buffer's file name relative to its project."
  (let ((file-name (buffer-file-name (dm-guard--selected-buffer))))
    (unless file-name
      (user-error "The selected test buffer is not visiting a file"))
    (file-relative-name file-name (project-root (project-current t)))))

(defun dm-guard-test ()
  "Run the test associated with the current buffer."
  (interactive)
  (cond
   ((not dm-guard-enabled)
    (when (called-interactively-p 'interactive)
      (user-error "Guard test runners are disabled")))
   ((not (project-current))
    (when (called-interactively-p 'interactive)
      (user-error "The current buffer does not belong to a project")))
   ((and dm-guard-line-mode-enabled (dm-guard--test-file-p))
    (dm-guard-test-line))
   (t
    (dm-guard-test-file))))

(defun dm-guard-test-file ()
  "Run the test file associated with the current buffer."
  (interactive)
  (let ((test-command (dm-guard--test-command))
        (test-name (dm-guard--test-name)))
    (if test-name
        (dm-guard--run test-command test-name)
      (message "No suitable test file found for %s" (buffer-name)))))

(defun dm-guard-test-line ()
  "Run the test at point in the current test file."
  (interactive)
  (let ((test-command (dm-guard--test-command))
        (test-name (dm-guard--test-name))
        (line (line-number-at-pos)))
    (if test-name
        (dm-guard--run test-command test-name line)
      (message "No suitable test file found for %s" (buffer-name)))))

(defun dm-guard--run (test-command test-name &optional line)
  "Run TEST-COMMAND against TEST-NAME, optionally restricted to LINE."
  (let* ((project (project-current t))
         (default-directory (file-name-as-directory (project-root project)))
         (target (unless (equal test-name "")
                   (if line (format "%s:%s" test-name line) test-name)))
         (command (if target
                      (concat test-command " " (shell-quote-argument target))
                    test-command))
         (buffer (get-buffer dm-guard-buffer-name)))
    (if (and (string-equal dm-guard--currently-running-test command)
             (process-live-p (and buffer (get-buffer-process buffer))))
        'already-running
      (require 'ghostel-compile)
      (setq dm-guard--currently-running-test command)
      (let ((compilation-always-kill t)
            (display-buffer-overriding-action
             '((display-buffer-in-side-window)
               (side . right)
               (window-width . 0.2)))
            (ghostel-compile-buffer-name dm-guard-buffer-name))
        (ghostel-compile command)))))

(defun dm-guard--rspec-test-command ()
  "Return the command used to run RSpec."
  (concat "bundle exec rspec --format=documentation --color"
          (if dm-guard-only-failures " --only-failures" "")))

(defun dm-guard--test-command ()
  "Return the test command appropriate for the selected buffer."
  (let ((project-type (project-type))
        (file-name (dm-guard--relative-file-name)))
    (cond
     ((dm-guard--javascript-file-p file-name) "yarn test")
     ((memq project-type '(rails-rspec ruby-rspec rubygem))
      (dm-guard--rspec-test-command))
     ((eq project-type 'rails-test) "bin/rails test")
     ((eq project-type 'ruby-test) "ruby")
     ((eq project-type 'swift-package) "swift test")
     (t "ruby"))))

(defun dm-guard--test-file-p (&optional buffer)
  "Return non-nil when BUFFER visits a recognized test file."
  (when-let* ((file-name (buffer-file-name buffer)))
    (or (string-match-p "\\(?:_\\|\\.\\)test\\.\\(?:rb\\|[jt]sx?\\)\\'" file-name)
        (string-match-p "_spec\\.rb\\'" file-name))))

(defun dm-guard--javascript-file-p (file-name)
  "Return non-nil when FILE-NAME names a JavaScript-family file."
  (string-match-p "[jt]sx?\\'" file-name))

(defun dm-guard--test-name ()
  "Return the test target associated with the selected buffer."
  (let* ((project-type (project-type))
         (spec-mode (project-verify-file "spec"))
         (test-buffer (dm-guard--selected-buffer))
         (test-file-p (dm-guard--test-file-p test-buffer))
         (file-name (dm-guard--relative-file-name)))
    (cond
     ((eq project-type 'swift-package) "")
     (test-file-p file-name)
     ((dm-guard--javascript-file-p file-name)
      (when (string-match "^app/\\(.+\\)\\.\\([jt]sx?\\)$" file-name)
        (concat "spec/" (match-string 1 file-name) ".test." (match-string 2 file-name))))
     ((string-match-p "^app/views" file-name) nil)
     ((string-match "^app/graphs/\\(.+\\)/vertices/\\(.+\\)_vertex\\.rb$" file-name)
      (let ((graph-directory (match-string 1 file-name))
            (vertex-name (match-string 2 file-name)))
        (if spec-mode
            (concat "spec/graphs/" graph-directory "/latest/vertices/"
                    vertex-name "_vertex_spec.rb")
          (concat "test/integration/pco/api/" graph-directory "/"
                  (dm-guard--pluralize vertex-name) "_test.rb"))))
     ((string-match "^app/graphs/\\(.+\\)\\.rb$" file-name)
      (if spec-mode
          (concat "spec/requests/graphs/" (match-string 1 file-name) "_spec.rb")
        (concat "test/integration" (match-string 1 file-name) "_test.rb")))
     ((string-match "^app/\\(.+\\)\\.rb$" file-name)
      (if spec-mode
          (concat "spec/" (match-string 1 file-name) "_spec.rb")
        (concat "test/" (match-string 1 file-name) "_test.rb")))
     ((string-match "^lib/\\(.+\\)\\.rb$" file-name)
      (if spec-mode
          (concat "spec/lib/" (match-string 1 file-name) "_spec.rb")
        (concat "test/" (match-string 1 file-name) "_test.rb")))
     ((string-match "^test/fixtures/\\(.+\\)\\.yml$" file-name)
      (concat "test/models/"
              (dm-guard--singularize (match-string 1 file-name))
              "_test.rb")))))

(defun dm-guard-select-test-buffer (buffer)
  "Use BUFFER as the test target for the current buffer."
  (interactive "bTest buffer: ")
  (setq-local dm-guard-manual-test-buffer (get-buffer buffer)))

(defun dm-guard-clear-test-buffer ()
  "Clear the manually selected test target for the current buffer."
  (interactive)
  (setq-local dm-guard-manual-test-buffer nil))

(defvar-keymap dm-guard-command-map
  :doc "Commands for running related tests."
  "l" #'dm-guard-test-line
  "t" #'dm-guard-test
  "b" #'dm-guard-select-test-buffer)

(defvar-keymap dm-guard-mode-map
  "C-c k" dm-guard-command-map)

(define-minor-mode dm-guard-mode
  "Run the test associated with the current buffer after saving."
  :init-value nil
  :lighter "􀙧"
  :keymap dm-guard-mode-map
  (if dm-guard-mode
      (add-hook 'after-save-hook #'dm-guard-test nil t)
    (remove-hook 'after-save-hook #'dm-guard-test t)))

(provide 'dm-guard)
;;; dm-guard.el ends here
