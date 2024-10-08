;;; dm-evil.el --- Evil
;;; Commentary:

;;; Code:

(use-package evil
  ;; :after undo-tree
  ;; Custom doesn't work with evil _for some reason_
  :init
  (setq evil-disable-insert-state-bindings t
        evil-default-state 'emacs
	evil-echo-state nil
	evil-symbol-word-search t
	evil-undo-system 'undo-tree
	evil-want-integration t
	evil-want-keybinding nil
	evil-mode-line-format '(before . mode-line-plz-put-evil-tag-here)
        evil-lookup-func 'eldoc-doc-buffer

        evil-normal-state-tag "N"
        evil-emacs-state-tag "E"
        evil-insert-state-tag "I")
  :config
  (evil-mode 1)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'rg-mode 'emacs)
  (evil-set-initial-state 'Custom-mode 'emacs)
  (evil-set-initial-state 'embark-collect-mode 'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs)
  :bind (:map evil-normal-state-map
              ("k" . evil-previous-visual-line)
              ("j" . evil-next-visual-line)
              ("C-." . nil)
              :map evil-insert-state-map
              ("C-o" . evil-execute-in-normal-state)))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init `(dired (magit magit-repos magit-submodule) magit-section magit-todos)) ; (seq-difference evil-collection-mode-list '(rg (custom cus-edit) vterm embark)))

;; (use-package evil-textobj-tree-sitter
;;   :after evil
;;   :config
;;   ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
;;   (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;;   ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
;;   (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;;   )

;; (treesit-query-compile 'tsx)
;; (treesit-query-validate 'tsx "(call_expression) @c")

;; (treesit-query-range
;;  (treesit-buffer-root-node 'tsx)
;;  "(call_expression) @c"
;;  )

(use-package evil-multiedit
  :bind (:map evil-visual-state-map
              ("R" . evil-multiedit-match-all)
              ("M-d" . evil-multiedit-match-and-next)
              ("M-D" . evil-multiedit-match-and-previous)
              :map evil-normal-state-map
              ("M-d" . evil-multiedit-match-and-next)
              ("M-D" . evil-multiedit-match-and-previous)
              ("C-M-D" . evil-multiedit-restore)
                                        ;     :map evil-multiedit-state-map
                                        ;     ("C-n" . evil-multiedit-next)
                                        ;     ("C-p" . evil-multiedit-prev)
                                        ;     :map evil-multiedit-insert-state-map
                                        ;     ("C-n" . evil-multiedit-next)
                                        ;     ("C-p" . evil-multiedit-prev)
              ))


(if t
    t

  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (defun system-paste ()
    "Always paste from the system clipboard."
    (interactive)
    (insert (copy-from-osx)))

  (defun system-yank (start end)
    "Always yank from the system clipboard."
    (interactive "r")
    (paste-to-osx (buffer-substring-no-properties start end))
    (evil-normal-state))

  (defun my/window-x-sibling-p ()
    (let ((w (get-buffer-window)))
      (or (windmove-find-other-window 'right) (windmove-find-other-window 'left))))

  (defun my/window-percent (percent)
    (interactive "N")
    (let ((p (* percent 0.1)))
      (if (my/window-x-sibling-p)
	  (evil-resize-window (round (* (frame-width) p)) t)
        (evil-resize-window (round (* (frame-height) p))))))

  (use-package evil
    ;; :after undo-tree
    ;; :custom
    ;; (evil-shift-width 2)
    ;; (evil-shift-round t)
    ;; (evil-echo-state nil)
    ;; (evil-symbol-word-search t)
    ;; (windmove-wrap-around t)
    :init
    (setq evil-want-integration t
	  evil-want-keybinding nil)
    :config
    (evil-mode 1)
    (evil-set-undo-system 'undo-tree)
    (define-key base-leader-map "w" evil-window-map)
    (defhydra hydra-evil-window ()
              "window"
              ("h" evil-window-left             "left")
              ("j" evil-window-down             "down")
              ("k" evil-window-up               "up")
              ("l" evil-window-right            "right")

              ("w" ace-window                   "ace-window")

              ;; movement
              ("H" evil-window-move-far-left    "far left")
              ("J" evil-window-move-very-bottom "far bottom")
              ("K" evil-window-move-very-top    "far top")
              ("L" evil-window-move-far-right   "far right")
              ("C-h" buf-move-left              "move left")
              ("C-j" buf-move-down              "move down")
              ("C-k" buf-move-up                "move up")
              ("C-l" buf-move-right             "move right")

              ;; size
              ("-" evil-window-decrease-height "height -")
              ("+" evil-window-increase-height "height +")
              ("<" evil-window-decrease-width  "width -")
              (">" evil-window-increase-width  "width +")
              ("p" my/window-percent "percent")

              ("_" evil-window-set-height      "height full")
              ("|" evil-window-set-width       "width full")
              ("=" balance-windows             "balance")
              )
    (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
    :bind (
           ("s-]" . evil-window-next)
           ("s-[" . evil-window-prev)
           ("M-s-∆" . evil-window-down)
           ("M-s-˚" . evil-window-up)
           ("M-s-˙" . evil-window-left)
           ("M-s-¬" . evil-window-right)
           ("s-v" . system-paste)
           ("s-c" . system-yank)
           :map evil-insert-state-map
           ("C-n" . next-line)
           ("C-p" . previous-line)
           ("C-a" . beginning-of-line-text)
           ("C-e" . end-of-line)
           ("C-d" . delete-forward-char)
           ("M-RET" . tt/expand-at-point)
           :map evil-normal-state-map
           ("M-RET" . newline)
           ("[b" . previous-buffer)
           ("]b" . next-buffer)
           ("C-." . my/m-x)
           ("j" . evil-next-visual-line)
           ("k" . evil-previous-visual-line)
           :map evil-visual-state-map
           ;; ("C-w" . interactive-wrap-with-pair)
           :map evil-window-map
           ("C-h" . buf-move-left)
           ("C-j" . buf-move-down)
           ("C-k" . buf-move-up)
           ("C-l" . buf-move-right)
           ("p" . my/window-percent)
           ("." . hydra-evil-window/body)
           :map base-leader-map
           ("," . evil-avy-goto-char)
           ))

  (use-package iedit)

  (use-package key-chord
    :after evil
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map ",," 'evil-avy-goto-char)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-normal-state-map "''" 'my/m-x))

  )

(provide 'dm-evil)
;;; dm-evil.el ends here
