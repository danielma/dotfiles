;;; dm-tabs --- My tabs configuration

;;; Commentary:

;;; Code:
(defun dm-tab-bar-name ()
  "Return the current buffer name padded or truncated to 20 columns."
  (let ((tab-name (buffer-name (window-buffer (minibuffer-selected-window)))))
    (concat " " (truncate-string-to-width tab-name 20 0 ?\s "…") " ")))

(use-package tab-bar
  ;; :init
  ;; (defhydra hydra-tab-bar (base-leader-map "t")
  ;;   "tabs"
  ;;   ("c" tab-bar-new-tab :exit t)
  ;;   ("n" tab-bar-switch-to-next-tab)
  ;;   ("p" tab-bar-switch-to-prev-tab)
  ;;   ("k" tab-bar-close-tab)
  ;;   ("t" tab-bar-switch-to-tab :exit t)
  ;;   ("j" tab-bar-select-tab-by-name :exit t)
  ;;   )
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-show t)
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-tab-name-function 'dm-tab-bar-name)
  :config
  (tab-bar-mode))
;; :bind (:map global-map
;; 	 ("s-t" . tab-bar-new-tab)
;; 	 ("s-w" . tab-bar-close-tab)
;; 	 ("s-{" . tab-bar-switch-to-prev-tab)
;; 	 ("s-}" . tab-bar-switch-to-next-tab)))

(provide 'dm-tabs)

;;; dm-tabs.el ends here
