(use-package elscreen
  :disabled
  :init
  (elscreen-start)
  :config
  (setq elscreen-display-screen-number nil)
  (setq elscreen-display-tab 30)
  (setq elscreen-tab-display-control nil)
  (setq elscreen-tab-display-kill-screen nil)
  (defhydra hydra-elscreen (base-leader-map "t")
    "screens"
    ("c" elscreen-create :exit t)
    ("n" elscreen-next)
    ("p" elscreen-previous)
    ("k" elscreen-kill)
    ("j" elscreen-select-and-goto :exit t)
    ("t" elscreen-toggle-display-tab)
    )
  :bind (:map global-map
	 ("s-t" . elscreen-create)
	 ("s-w" . elscreen-kill)
	 ("s-{" . elscreen-previous)
	 ("s-}" . elscreen-next)))

(defun dm-tab-bar-name ()
  "Consistent width tab names for `tab-bar-mode`."
  (let ((tab-name (buffer-name (window-buffer (minibuffer-selected-window)))))
    (concat " " (s-truncate 20 (s-pad-right 20 " " tab-name)) " ")))

(use-package tab-bar
  :init
  (setq tab-bar-new-tab-choice "*scratch*"
        tab-bar-show 1
        tab-bar-new-button-show nil
        tab-bar-close-button-show nil
        tab-bar-tab-name-function 'dm-tab-bar-name)
  (defhydra hydra-tab-bar (base-leader-map "t")
    "tabs"
    ("c" tab-bar-new-tab :exit t)
    ("n" tab-bar-switch-to-next-tab)
    ("p" tab-bar-switch-to-prev-tab)
    ("k" tab-bar-close-tab)
    ("j" tab-bar-select-tab-by-name :exit t)
    )
  :config
  (tab-bar-mode)
  :bind (:map global-map
	 ("s-t" . tab-bar-new-tab)
	 ("s-w" . tab-bar-close-tab)
	 ("s-{" . tab-bar-switch-to-prev-tab)
	 ("s-}" . tab-bar-switch-to-next-tab)))

(provide 'dm-tabs)
