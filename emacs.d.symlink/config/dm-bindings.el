(defvar base-leader-map nil "The main LEADER map.")

(use-package bind-map
  :config
  (bind-map base-leader-map
    :override-minor-modes t
    :keys ("M-m")
    :evil-keys ("SPC"))

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

    ;; "mw" 'web-mode
    ;; "mj" 'js-mode

    "ss" 'evil-search-word-forward
    "sr" 'replace-symbol
    "sa" 'find-symbol-in-project

    "T" text-tools-map

    "," 'ace-jump-char-mode)
  )

(use-package hydra
  :config
  (defhydra hydra-buffers ()
    "buffers"				;
    ("p" previous-buffer "previous")
    ("n" next-buffer "next"))
  :bind (:map base-leader-map
	      ("b." . hydra-buffers/body)))

(provide 'dm-bindings)
