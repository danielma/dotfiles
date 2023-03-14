;;; dm-term.el --- Terminal config

;;; Commentary:

;;; Code:

(require 'dm-projects)

(defun with-editor-advice-around (orig-fun &rest args)
  "Wrap ORIG-FUN with-editor and pass ARGS."
  (with-editor
    ;; (print process-environment)
    (apply orig-fun args)))

(use-package vterm
  :config
  (advice-add 'vterm :around 'with-editor-advice-around)
  (customize-set-variable 'vterm-keymap-exceptions (append vterm-keymap-exceptions '("M-h" "M-j" "M-k" "M-l")))
  :bind (:map vterm-mode-map
              ("C-c C-c" . vterm-send-C-c))
  )

(declare-function vterm "vterm")
(defvar vterm-buffer-name)

;; (with-eval-after-load 'vterm
;;   (with-eval-after-load 'evil
;;     (evil-set-initial-state 'vterm-mode 'emacs)
;;     ))


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

(defun switch-to-term ()
  "Switch to a term buffer for the current project."
  (interactive)
  (if (derived-mode-p 'eat-mode 'vterm-mode)
      (previous-buffer)
    (let* ((bufname (if (project-name)
                        (concat (s-chop-right 1 vterm-buffer-name) "-" (project-name) "*")
                      vterm-buffer-name))
           (existing-buffer (get-buffer bufname)))
      (if existing-buffer (display-buffer existing-buffer) (vterm bufname)))))
;; (let ((eat-buffer (get-buffer (and (boundp 'eat-buffer-name) eat-buffer-name))))
;;   (if eat-buffer
;;       (display-buffer eat-buffer)
;;     (eat)))))

(use-package emacs
  :bind (:map global-map
              ("s-T" . switch-to-term)))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter)) 

(provide 'dm-term)

;;; dm-term.el ends here
