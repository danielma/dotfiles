;;; dm-projects.el --- -*- lexical-binding: t -*-

;; (dir-locals-set-class-variables
;;  'login
;;  '(
;;    (ruby-mode . ((prettier-js-command . "prettier_d")
;;                  (prettier-js-args . ("--plugin" "/Users/danielma/.config/yarn/global/node_modules/@prettier/plugin-ruby"))
;;                  (eval . (prettier-js-mode))))
;;    ))


;; (dir-locals-set-directory-class "~/Code/login" 'login)

;; (evil-set-initial-state 'rg-mode 'emacs)

(use-package rg
  :init
  (rg-enable-default-bindings)
  :custom
  (rg-custom-type-aliases '(("yuh" . "*")))
  :config
  (rg-define-search rg-project-simple-literal "Simple Literal"
    :format literal
    :files "everything"
    :dir project
    :menu ("Search" "l" "Simple literal"))
  )

;;; Project

(defun project-name (&optional project)
  "Get the name of PROJECT."
  (let ((project (or project (project-current nil))))
    (if project
	      (let ((root (project-root project)))
	        (file-name-nondirectory (directory-file-name root))))))

                                        ; useful from Projectile
(defun project-verify-file (file)
  "Check whether FILE exists in the current project."
  (let ((root (project-root (project-current))))
    (file-exists-p (expand-file-name file root))))

(defun project-find-test-or-implementation (&optional buffer)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (filename (buffer-file-name buffer))
         (basename (file-name-base filename))
         (project (project-current t))
         (root (project-root project))
         (maybe-test-dirs (list (file-name-concat root "spec") (file-name-concat root "test"))) ;; (file-name-concat root "test"))
         (test-dirs (-filter #'file-directory-p maybe-test-dirs))
         (fs (project-files project test-dirs))
         (matches (-filter (lambda (f) (s-contains? basename f)) fs)))
    (cl-case (length matches)
      (0 (message "No matches!"))
      (1 (find-file (car matches)))
      (t (find-file (funcall project-read-file-name-function "Alternate " matches nil 'file-name-history))))))

;;; End Project

(provide 'dm-projects)
