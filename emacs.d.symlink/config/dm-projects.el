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

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; Project

(use-package emacs
  :bind (:map project-prefix-map
              ("t" . #'project-find-test-or-implementation)
              ("T" . #'project-find-test-or-implementation-other-window)))

(defun project-name (&optional project)
  "Get the name of PROJECT."
  (let ((project (or project (project-current nil))))
    (if project
        (let ((root (project-root project)))
          (file-name-nondirectory (directory-file-name root))))))

;; useful from Projectile
(defun project-verify-file (file)
  "Check whether FILE exists in the current project."
  (let ((root (project-root (project-current))))
    (file-exists-p (expand-file-name file root))))

(defun project-verify-files (&rest files)
  "Check whether FILES exist in the current project."
  (-every #'project-verify-file files))

(defmacro with-project (project &rest body)
  "Execute BODY with PROJECT set."
  `(let ((project (or ,project (project-current nil))))
     (if project ,@body)))

(defvar-local project-project-type nil)

(defvar project-type-mappings
  '((rails-rspec . ((test . "spec") (impl . "app")))
    (rails-test  . ((test . "test") (impl . "app")))))

(defun project-type-mapping (&optional project)
  "Get mappings for PROJECT."
  (with-project project
                (let ((type (project-type project)))
                  (cdr (assoc type project-type-mappings)))))

(defun project--ruby-project-type ()
  "Get the Ruby project type."
  (if (or
       (project-verify-file "Gemfile")
       ;; TODO: verify *.gemspec
       )
      (if (project-verify-files "app" "config.ru")
          (cond ((project-verify-file "spec") 'rails-rspec)
                ((project-verify-file "test") 'rails-test))
        (cond ((project-verify-file "spec") 'ruby-rspec)
              ((project-verify-file "test") 'ruby-test)))))

(defun project-type (&optional project)
  "Get the type of PROJECT."
  (with-project project
                (or project-project-type
                    (setq project-project-type
                          (or (project--ruby-project-type)
                              'unknown)))))

(defun project-find-test-or-implementation (&optional buffer find-file-func)
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (find-file-func (or find-file-func 'find-file))
         (filename (buffer-file-name buffer))
         (matches (project-test-or-implementation filename)))
    (cl-case (length matches)
      (0 (message "No matches!"))
      (1 (funcall find-file-func (car matches)))
      (t (funcall find-file-func
                  (funcall project-read-file-name-function "Alternate " matches nil 'file-name-history))))))

(defun project-find-test-or-implementation-other-window (&optional buffer)
  (interactive)
  (project-find-test-or-implementation buffer 'find-file-other-window))

(defun project-test-or-implementation (&optional filename)
  "Find alternate for a given FILENAME."
  (or
   (when-let ((filename (or filename (buffer-file-name (current-buffer))))
              (basename (file-name-base filename))
              (project (project-current t))
              (project-type (project-type project))
              (root (project-root project))
              (project-type-mapping (project-type-mapping project))
              (test-dir-basename (cdr (assoc 'test project-type-mapping)))
              (impl-dir-basename (cdr (assoc 'impl project-type-mapping)))
              (test-dir (file-name-concat root test-dir-basename))
              (impl-dir (file-name-concat root impl-dir-basename))

              (test-suffix (concat "_" test-dir-basename)))
     (if (s-ends-with? test-suffix basename)
         (let ((impl-basename (s-chop-suffix test-suffix basename))
               (fs (project-files project (list impl-dir))))
           (-filter (lambda (f) (s-contains? impl-basename f)) fs))
       (let ((test-basename (concat basename "_" test-dir-basename))
             (fs (project-files project (list test-dir))))
         (-filter (lambda (f) (s-contains? test-basename f)) fs))))
   (list)))

;;; End Project

(provide 'dm-projects)
