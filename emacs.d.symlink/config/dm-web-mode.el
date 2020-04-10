(defun my-web-mode-setup ()
  (setq-local electric-indent-chars
              (append "{};" electric-indent-chars))
  (if (equal web-mode-engine "php")
      (my/web-mode-php-setup))
  (if (member web-mode-engine '("php" "erb"))
      (modify-syntax-entry ?_ "w"))
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx"))
  (if (s-matches? "tsx?$" (file-name-extension (buffer-file-name)))
      (setup-tide-mode))
  )

(defun my/web-mode-php-setup ()
  "Web mode setup for php."
  )

(defun my/web-mode-control-colon ()
  (interactive)
  (end-of-line)
  (insert ";")
  (newline-and-indent))

(defun my/flycheck-enable-javascript-eslint (orig-fun)
  (if (eq major-mode 'web-mode)
      (and
       (equal web-mode-content-type "jsx")
       (funcall orig-fun))
    (funcall orig-fun)))

(use-package web-mode
  :after flycheck
  :commands web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html\\(\+modal\\)?\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.module\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
  (add-hook 'web-mode-hook 'my-web-mode-setup)
  (setq original-flycheck-javascript-eslint-enabled (flycheck-checker-get 'javascript-eslint 'enabled))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (define-abbrev-table 'web-mode-abbrev-table '(
						("nd" "<% end %>")
						("tt" "<%")
						("tp" "<%=")))
  (dolist (width '(web-mode-attr-indent-offset web-mode-code-indent-offset web-mode-css-indent-offset web-mode-markup-indent-offset web-mode-sql-indent-offset))
    (set width 2))
  (setq web-mode-engines-alist

	'(("php" . "\\.module\\'")))
  (setf (flycheck-checker-get 'javascript-eslint 'enabled)
        (lambda ()
          (my/flycheck-enable-javascript-eslint original-flycheck-javascript-eslint-enabled)))
  :bind (:map web-mode-map
	      ("C-:" . my/web-mode-control-colon))
  )

     ;; (defun his-tracing-function (orig-fun &rest args)
     ;;   (message "display-buffer called with args %S" args)
     ;;   (let ((res (apply orig-fun args)))
     ;;     (message "display-buffer returned %S" res)
     ;;     res))
     
     ;; (advice-add 'display-buffer :around #'his-tracing-function)

(use-package emmet-mode
  :init
  (add-hook 'web-mode-hook 'emmet-mode)
  :config
  (setq emmet-indentation 2))

(provide 'dm-web-mode)
