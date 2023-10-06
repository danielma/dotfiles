;;; dm-remote-repositories --- Kinda like the VSCode extension -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'magit)
(require 's)

(defun remote-repository-explore (name)
  "Explore repository with NAME."
  (interactive
   (list
    (completing-read "Repository URL or Github reference: " remote-repository-excursions)))
  (let* ((repo-url (cond ((s-match "^\\(https\\|git@.+\\):" name) name)
                         (t (concat "git@github.com:" name ".git"))))
         (magit-clone-default-directory (make-temp-file (remote-repository--escape-url repo-url) t))
         (magit-clone-set-remote.pushDefault t)
         (existing-excursion (assoc repo-url remote-repository-excursions)))
    (if existing-excursion
        (magit-status (cdr existing-excursion))
      (if (zerop (magit-process-git nil "ls-remote" "--exit-code" repo-url))
          (progn
            (magit-clone-regular repo-url magit-clone-default-directory '())
            (add-to-list 'remote-repository-excursions `(,repo-url . ,magit-clone-default-directory)))
        (error (concat "Could not find repository at: " repo-url))))))

(defvar remote-repository-excursions
  '()
  "Saved list of excursions for the current session.")

(defun remote-repository--escape-url (url)
  "Escape URL to make it safe for a directory."
  (let ((repo-name (cadr (s-match ":\\(?://github\\.com/\\)?\\(.+?\\)\\(?:\\.git\\)?$" url))))
    (replace-regexp-in-string "[^[:alnum:]]" "_" repo-name)))

(provide 'dm-remote-repositories)

;;; dm-remote-repositories.el ends here
