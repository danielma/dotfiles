(require 'projectile)

(projectile-global-mode)
(require 'helm-projectile)
(helm-projectile-on)

(with-eval-after-load 'helm-projectile
  (defvar helm-source-file-not-found
    (helm-build-dummy-source
        "Create file"
      :action (lambda (cand) (find-file cand))))

  (add-to-list 'helm-projectile-sources-list helm-source-file-not-found t))

(defun projectile-set-default-directory ()
  (interactive)
  (cd (projectile-project-root))
  (print (concat "Set root to " (projectile-project-root))))

(define-key projectile-command-map "T" 'projectile-find-implementation-or-test-other-window)
(define-key projectile-command-map "a" 'helm-projectile-ag)
(define-key projectile-command-map "C" 'projectile-set-default-directory)
