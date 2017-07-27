(setq base-leader-key "M-m"
      base-leader-evil-key "SPC")

(defun my/leader-sub-key (key)
  (list (concat base-leader-key " " key)))

(defun my/leader-evil-sub-key (key)
  (list (concat base-leader-evil-key " " key)))

(use-package bind-map
  :config
  (eval
   `(bind-map base-leader-map
      :keys ,(list base-leader-key)
      :evil-keys ,(list base-leader-evil-key)
      :evil-states (normal motion visual)))

  (bind-map-set-keys base-leader-map
    "fs" 'save-buffer-always
    "fq" 'delete-window
    "fr" 'force-reload

    "bd" 'kill-this-buffer
    "bs" 'helm-buffers-list

    "cd" 'cd
    "cl" 'custom-comment-line
    "ct" 'my/base16-set-theme
    "cf" 'my/set-custom-face

    "dr" 'reveal-in-finder

    "ee" 'edit-emacs
    "es" 'edit-scratch
    "ey" 'edit-yasnippet-dir

    "gB" 'browse-at-remote

    "hr" 'helm-resume
    "hk" 'helm-show-kill-ring

    "ll" 'custom-flycheck-toggle-errors
    "ln" 'flycheck-next-error
    "lp" 'flycheck-previous-error

    ;; "mw" 'web-mode
    ;; "mj" 'js-mode

    "ss" 'evil-search-word-forward
    "sr" 'replace-symbol
    "sa" 'find-symbol-in-project

    "T" text-tools-map

    "," 'ace-jump-char-mode))

(provide 'dm-bindings)
