;;; dm-term.el --- Terminal config

;;; Commentary:

;;; Code:

(defun with-editor-advice-around (orig-fun &rest args)
  "Wrap ORIG-FUN with-editor and pass ARGS."
  (with-editor
    ;; (print process-environment)
    (apply orig-fun args)))

(use-package vterm
  :config
  (advice-add 'vterm :around 'with-editor-advice-around)
  )

(with-eval-after-load 'vterm
  (with-eval-after-load 'evil
    (evil-set-initial-state 'vterm-mode 'emacs)
    ))


(use-package eat
  :disabled
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
  (advice-add 'eat :around 'with-editor-advice-around)
  )

(defun switch-to-eat ()
  (interactive)
  (if (derived-mode-p 'eat-mode 'vterm-mode)
      (previous-buffer)
    (vterm)))
;; (let ((eat-buffer (get-buffer (and (boundp 'eat-buffer-name) eat-buffer-name))))
;;   (if eat-buffer
;;       (display-buffer eat-buffer)
;;     (eat)))))

(use-package emacs
  :bind (:map global-map
              ("s-T" . switch-to-eat)))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter)) 

(provide 'dm-term)

;;; dm-term.el ends here
