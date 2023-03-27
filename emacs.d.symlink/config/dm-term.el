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
  :custom
  (vterm-keymap-exceptions '("C-c" "C-x" "C-u" "C-g" "C-h" "C-l" "M-x" "M-o" "C-y" "M-y" "M-h" "M-j" "M-k" "M-l"))
  :config
  (advice-add 'vterm :around 'with-editor-advice-around)

  (defadvice process-kill-buffer-query-function (before vterm-set-process-query-on-exit-flag-before-query activate)
    "Set `process-query-on-exit-flag' correctly for vterm buffers."
    (let ((process (get-buffer-process (current-buffer))))
      (and process
           (vterm-check-proc)
           (set-process-query-on-exit-flag process (process-running-child-p process)))
      t))
  :bind (:map vterm-mode-map
              ("s-v" . vterm-yank-with-clipboard))
  )

(declare-function vterm "vterm")
(declare-function vterm-yank "vterm")
(defvar vterm-buffer-name)

(defun vterm-yank-with-clipboard ()
  "Vterm Yank from the system clipboard."
  (interactive)
  (let ((select-enable-clipboard t))
    (vterm-yank)))

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
