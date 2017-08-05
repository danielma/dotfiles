;; (progn
;;   (setq mode-line-format
;;         '("%e" mode-line-front-space " " mode-line-client mode-line-modified mode-line-remote " " mode-line-buffer-identification mode-line-position evil-mode-line-tag
;;            mode-name mode-line-misc-info "  " mode-line-end-spaces))
;;   (force-mode-line-update))

;; (defun add-mode-line-dirtrack ()
;;     "When editing a file, show the last 2 directories of the current path in the mode line."
;;     (add-to-list 'mode-line-buffer-identification
;;                  '(:eval (substring default-directory
;;                                     (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))
;; (add-hook 'find-file-hook 'add-mode-line-dirtrack)

(defface mode-line-powerline-symbol
  '((t (:height 1.2)))
  "Face used to make powerline symbols a little taller."
  :group 'mode-line-powerline)

(defface mode-line-evil
  '((t (:foreground "white" :weight bold :inherit mode-line)))
  "Meta-face used for property inheritance on all mode-line-evil faces."
  :group 'mode-line-evil)

(defface mode-line-evil-insert
  '((t (:background "green" :inherit mode-line-evil)))
  "Face used in evil color-coded segments when in Insert state."
  :group 'mode-line-evil)

(defface mode-line-evil-normal
  '((t (:background "red" :inherit mode-line-evil)))
  "Face used in evil color-coded segments when in Normal state."
  :group 'mode-line-evil)

(defface mode-line-evil-visual
  '((t (:background "orange" :inherit mode-line-evil)))
  "Face used in evil color-coded segments when in Visual{,-Block,-Line} state."
  :group 'mode-line-evil)

(defface mode-line-evil-replace
  '((t (:background "black" :inherit mode-line-evil)))
  "Face used in evil color-coded segments when in Replace state."
  :group 'mode-line-evil)

(defface mode-line-evil-motion
  '((t (:background "blue" :inherit mode-line-evil)))
  "Face used in evil color-coded segments when in Motion state."
  :group 'mode-line-evil)

(defface mode-line-evil-operator
  '((t (:background "violet" :inherit mode-line-evil)))
  "Face used in evil color-coded segments when in Operator state."
  :group 'mode-line-evil)

(defface mode-line-evil-emacs
  '((t (:background "dark violet" :inherit mode-line-evil)))
  "Face used in evil color-coded segments when in Emacs state."
  :group 'mode-line-evil)

(defface mode-line-accent-active
  '((t (:inherit mode-line)))
  "Accented mode-line segment"
  :group 'mode-line)

(defface mode-line-accent-inactive
  '((t (:inherit mode-line-inactive)))
  "Inactive accented mode-line segment"
  :group 'mode-line)

(defface mode-line-dark-active
  '((t (:inherit mode-line)))
  "Darked mode-line segment"
  :group 'mode-line)

(defface mode-line-flycheck-error
  '((t (:background "red" :foreground "white" :inherit mode-line)))
  "Flycheck error face"
  :group 'mode-line-flycheck)

(defface mode-line-flycheck-warning
  '((t (:background "orange" :foreground "white" :inherit mode-line)))
  "Flycheck warning face"
  :group 'mode-line-flycheck)

(defface mode-line-flycheck-info
  '((t (:background "yellow" :foreground "white" :inherit mode-line)))
  "Flycheck info face"
  :group 'mode-line-flycheck)

(defface mode-line-flycheck-ok
  '((t (:background "green" :foreground "white" :inherit mode-line)))
  "Flycheck ok face"
  :group 'mode-line-flycheck)

(defface mode-line-flycheck-running
  '((t (:background "blue" :foreground "white" :inherit mode-line)))
  "Flycheck running face"
  :group 'mode-line-flycheck)

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) 2)))
    (format (format "%%s %%%ds" available-width) left right)))

(defvar mode-line-selected-window nil)

(defun my/mode-line--set-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq mode-line-selected-window (frame-selected-window))))

(add-hook 'window-configuration-change-hook #'my/mode-line--set-selected-window)
(add-hook 'focus-in-hook #'my/mode-line--set-selected-window)
(defadvice select-window (after mode-line-select-window activate)
  "Set mode-line's selected window value for use in determining the active mode-line."
  (my/mode-line--set-selected-window))
(defadvice select-frame (after mode-line-select-frame activate)
  "Set mode-line's selected window value for use in determining the active mode-line."
  (my/mode-line--set-selected-window))

(defun my/mode-line-selected-window-active ()
  "Return whether the current window is active."
  (eq mode-line-selected-window (selected-window)))

(defun my/mode-line-face (sym)
  "Function for getting the face for a group (doesn't require active)."
  (my/mode-line--face sym (my/mode-line-selected-window-active)))

(defun my/mode-line-evil-face (active)
  "Return an appropriate face for the current evil mode, given whether the frame is ACTIVE."
  (cond ((not active) 'mode-line-inactive)
        ((not (boundp 'evil-state)) 'mode-line)
        (t (intern (concat "mode-line-evil-" (symbol-name evil-state))))))

(defun my/mode-line-evil-tag ()
  "Return an abbreviated evil tag for the current state."
  (pcase evil-state
    ('normal "N")
    ('insert "I")
    ('visual "V")
    ('replace "R")
    ('motion "M")
    ('emacs "E")
    ('operator "O")
    (_ (symbol-name evil-state))))

(defun my/mode-line-flycheck-error-level ()
  "Return current error level for flycheck."
  (cond ((flycheck-has-current-errors-p 'error) "error")
        ((flycheck-has-current-errors-p 'warning) "warning")
        ((flycheck-has-current-errors-p 'info) "info")
        (t "ok")))

(defun my/mode-line-flycheck-face (state)
  "Return an appropriate face for the flycheck STATE."
  (let ((active (my/mode-line-selected-window-active)))
    (cond ((not active) 'mode-line-inactive)
          ((not (bound-and-true-p flycheck-mode)) 'mode-line)
          (t (intern (concat "mode-line-flycheck-" (symbol-name state)))))))

(defun my/mode-line--face (sym active)
  "Return the face corresponding to SYM for the given ACTIVE state."
  (cond ((eq sym 'evil) (my/mode-line-evil-face active))
        ((eq sym 'dark) (if active 'mode-line-dark-active
                            'mode-line-inactive))
        ((eq sym 'accent) (if active 'mode-line-accent-active
                            'mode-line-accent-inactive))
        (active 'mode-line)
        (t 'mode-line-inactive)))

(defun my/flycheck-count-errors (kind)
  "Return count of flycheck errors for KIND."
  (let ((errors (flycheck-count-errors flycheck-current-errors)))
    (or (cdr (assq kind errors)) 0)))

(defun my/mode-line-flycheck-wrap (text face)
  "Return propertized TEXT with a FACE and powerline characters."
  `(
    ,(my/mode-line-powerline-propertize (my/mode-line-face 'normal) "\ue0b2" face)
    (:propertize ,(concat " " text " ") face ,face)
    ,(my/mode-line-powerline-propertize face "\ue0b2" (my/mode-line-face 'normal))
    )
  )

(defun my/mode-line-flycheck-segment (kind)
  "Return propertized mode line segment for KIND."
  (cond ((eq kind 'ok)
	 (my/mode-line-flycheck-wrap "\u2713" (my/mode-line-flycheck-face 'ok)))
	((eq kind 'running)
	 (my/mode-line-flycheck-wrap "\u2219" (my/mode-line-flycheck-face 'running)))
	((flycheck-has-current-errors-p kind)
	 (my/mode-line-flycheck-wrap (number-to-string (my/flycheck-count-errors kind)) (my/mode-line-flycheck-face kind)))
	(t "")))

(defun my/mode-line-flycheck-status ()
  "Full flycheck mode line string."
  (let ((errors (flycheck-count-errors flycheck-current-errors)))
    (cond (errors
           `(
             ,(my/mode-line-flycheck-segment 'error)
             ,(my/mode-line-flycheck-segment 'warning)
             ,(my/mode-line-flycheck-segment 'info)
             ))
          ((flycheck-running-p) (my/mode-line-flycheck-segment 'running))
	  (flycheck-enabled-checkers (my/mode-line-flycheck-segment 'ok)))))

(defvar mode-line-guard-status 'ok "The current guard status for mode-line.")

(defun my/mode-line-guard-status ()
  "Full guard mode line string."
  (if (eq mode-line-guard-status 'ok)
      `("" (:propertize " \u2694 " face ,(my/mode-line-face 'accent)))
    `("" (:propertize " \u2694 " face ,(my/mode-line-flycheck-face 'error)))
    ))

(setq mode-line-guard-status 'ok)

(defun my/mode-line-powerline-propertize (left-face text right-face)
  "Take LEFT-FACE, TEXT, and RIGHT-FACE and return a propertized list with appropriate foreground and background."
  `(:propertize ,text face (:background ,(face-attribute left-face :background) :foreground ,(face-attribute right-face :background) :inherit mode-line-powerline-symbol))
  )
;; use the function in conjunction with :eval and format-mode-line in your mode-line-format
;; (progn
  (setq-default mode-line-format
        '((:eval (simple-mode-line-render
                  ;; left
                  (format-mode-line `(
                                      "%e"
                                      ("" (:propertize ("  " (:eval (my/mode-line-evil-tag)) " ") face ,(my/mode-line-face 'evil)))
				      ,(my/mode-line-powerline-propertize (my/mode-line-face 'normal) "\ue0b0" (my/mode-line-face 'evil))
				      " "
                                      mode-line-client
                                      mode-line-modified
                                      " "
				      ,(if (projectile-project-p)
					 `(""
					   ,(my/mode-line-powerline-propertize (my/mode-line-face 'accent) "\ue0b0" (my/mode-line-face 'normal))
					   (:propertize
					    (" " (:eval (projectile-project-name)) " ")
					    face
					    ,(my/mode-line-face 'accent)
					    )
					   ,(my/mode-line-powerline-propertize (my/mode-line-face 'normal) "\ue0b0" (my/mode-line-face 'accent))))
				      " "
                                      mode-line-buffer-identification
                                      ))

                  ;; right
                  (format-mode-line `("%e"
                                      ;;,(my/mode-line-guard-status)
                                      ,(my/mode-line-flycheck-status)
                                      " "
                                      mode-name
                                      " "
				      ,(my/mode-line-powerline-propertize (my/mode-line-face 'normal) "\ue0b2" (my/mode-line-face 'accent))
                                      (""
                                       (:propertize
                                        (,(format-time-string " %I:%M  "))
                                        face ,(my/mode-line-face 'accent)))
                                      ))
                  ))))
  ;; (force-mode-line-update))
 
(provide 'dm-mode-line)
