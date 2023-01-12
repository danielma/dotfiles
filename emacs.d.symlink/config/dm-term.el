;;; dm-term.el --- Terminal config

;;; Commentary:

;;; Code:

(defun eat-with-editor (orig-fun &rest args)
  "Wrap ORIG-FUN with-editor and pass ARGS."
  (with-editor
    ;; (print process-environment)
    (apply orig-fun args)))

(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el")))
  :custom
  (eat-kill-buffer-on-exit t)
  :config
  (advice-add 'eat :around 'eat-with-editor)
  )

(defun switch-to-eat ()
  (interactive)
  (if (eq major-mode 'eat-mode)
      (previous-buffer)
    (let ((eat-buffer (get-buffer (and (boundp 'eat-buffer-name) eat-buffer-name))))
      (if eat-buffer
          (display-buffer eat-buffer)
        (eat)))))

(use-package emacs
  :bind (:map global-map
              ("s-T" . switch-to-eat)))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter)) 

(provide 'dm-term)

;;; dm-term.el ends here
