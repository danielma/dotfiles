;;; dm-modeline-test.el --- Tests for dm-modeline -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)

(load (expand-file-name
       "../config/dm-modeline.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest dm-modeline-places-project-before-buffer ()
  (let* ((format (default-value 'mode-line-format))
         (project-position
          (seq-position format
                        '(project-mode-line project-mode-line-format)
                        #'equal))
         (buffer-position
          (seq-position format 'mode-line-buffer-identification)))
    (should project-position)
    (should buffer-position)
    (should (< project-position buffer-position))))

(ert-deftest dm-modeline-replaces-vc-status-with-git-branch ()
  (let ((format (default-value 'mode-line-format)))
    (should-not (member '(vc-mode vc-mode) format))
    (should (member '(:eval (dm-modeline-git-branch)) format))))

(ert-deftest dm-modeline-caches-current-git-branch ()
  (with-temp-buffer
    (setq default-directory temporary-file-directory)
    (cl-letf (((symbol-function 'magit-get-current-branch)
               (lambda () "main")))
      (dm-modeline-update-git-branch))
    (should (equal dm-modeline-git-branch "main"))
    (should (equal (substring-no-properties (dm-modeline-git-branch))
                   " main"))))

;;; dm-modeline-test.el ends here
