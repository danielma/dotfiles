;;; dm-projects.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(dir-locals-set-class-variables
 'people '((ruby-mode . ((apheleia--syntax-tree-single-quotes . nil)))))

(dir-locals-set-directory-class "~/Code/people" 'people)

(defun my/rg-mode-setup ()
  (setq-local compilation-scroll-output nil)
  )

(use-package rg
  :init
  (rg-enable-default-bindings)
  :custom
  (rg-custom-type-aliases '(("yuh" . "*")))
  :hook
  (rg-mode . my/rg-mode-setup)
  :config
  (rg-define-search rg-project-simple-literal "Simple Literal"
    :format literal
    :files "everything"
    :dir project
    :menu ("Search" "l" "Simple literal"))
  )

(use-package editorconfig
  :ensure t
  :delight
  :config
  (editorconfig-mode 1))

;;; Compile

(defun compile-match-buffer-name-to-command ()
  ""
  (interactive)
  (rename-buffer (concat "*" compile-command "*")))

;;; Project

(use-package emacs
  :bind (:map project-prefix-map
              ("t" . #'project-find-test-or-implementation)
              ("T" . #'project-find-test-or-implementation-other-window)))

(add-to-list 'project-switch-commands '(switch-to-term "VTerm" ?t))
(add-to-list 'project-switch-commands '(rg-project-simple-literal "RG" ?g))
(add-to-list 'project-switch-commands '(magit-project-status "Magit" ?m))

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

(defmacro with-project-default-directory (&rest body)
  "Execute BODY with project root as default directory."
  `(let ((default-directory (project-root (project-current))))
     ,@body))

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

(defun project--swift-project-type ()
  "Get the Swift project type."
  (if (project-verify-file "Package.swift") 'swift-package))

(defun project-type (&optional project)
  "Get the type of PROJECT."
  (with-project project
                (or project-project-type
                    (setq project-project-type
                          (or (project--ruby-project-type)
                              (project--swift-project-type)
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

(defun project-choices (dirs)
  "Find files in directories where DIRS is a (dir re) pair."
  (let* ((project (project-current t))
         (root (expand-file-name (project-root project)))
         (hash (make-hash-table :test 'equal)))
    (loop for (dir re) in dirs do
          (let ((abs-dir (file-name-concat root dir)))
            (if (file-exists-p abs-dir)
                (loop for file in (project-files project (list (file-name-concat root dir))) do
                      (message "root: %s file: %s" root file)
                      (let ((rel-file (string-remove-prefix root file)))
                        (when (string-match re rel-file)
                          (puthash (match-string 1 rel-file) rel-file hash)))))))
    hash))

;;; End Project

(provide 'dm-projects)
