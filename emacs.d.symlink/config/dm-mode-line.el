;;; dm-mode-line.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar mode-line-powerline-bracket-right
  "\ue0b1")

(defvar mode-line-powerline-bracket-left
  "\ue0b3")

(defvar mode-line-powerline-separator-right
  "\ue0b0")

(defvar mode-line-powerline-separator-left
  "\ue0b2")

(defvar mode-line-plz-put-evil-tag-here
  "")

(defun my/mode-line-powerline-highlight (source &optional where)
  "Highlight SOURCE with powerline separators WHERE (right, left, or default both)."
  (let ((where (or where 'both)))
    (cl-case where
      ('both
       (list
	      (propertize (concat mode-line-powerline-separator-right " " source " ") 'face '(:inverse-video t))
	      mode-line-powerline-separator-right))
      ('right
       (list
	      (propertize (concat " " source " ") 'face '(:inverse-video t))
	      mode-line-powerline-separator-right))
      ('left
       (list
	      (propertize (concat mode-line-powerline-separator-right " " source " ") 'face '(:inverse-video t))))
      (t (error "WHERE must be right left or both")))))

(use-package emacs
  :custom
  (mode-line-format
   '("%e" ;; mode-line-front-space
     mode-line-plz-put-evil-tag-here
     " "
     (:propertize
      ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
      display
      (min-width
       (5.0)))
     ;;; mode-line-frame-identification
     " " mode-line-powerline-bracket-right " "
     mode-line-buffer-identification
     " "
     (:eval (if (project-name)
		            (my/mode-line-powerline-highlight (project-name))))
     "   " mode-line-position
     ;; (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
  (mode-line-percent-position '(6 "%q")))

(use-package delight
  :delight
  (auto-revert-mode)
  (evil-collection-unimpaired-mode)
  (eldoc-mode))

;;; Evil state tag

(with-eval-after-load 'evil
  (defun my/evil-generate-mode-line-tag (orig-fun &rest args)
    "Wrap around ORIG-FUN with ARGS to generate a better evil tag."
    (let (; (state-s (symbol-name (car args)))
	        (orig-tag (apply orig-fun args)))
      (if orig-tag
	        (let* ((tag (substring-no-properties orig-tag))
		             (clean-tag (car (s-match "\\w+" tag))))
                                        ; (face (intern (concat "mode-line-evil-state-" state-s))))
	          (my/mode-line-powerline-highlight clean-tag 'right)))))
	;; (list
	;;  ;; (propertize mode-line-powerline-separator-right 'face face)
	;;  (propertize tag 'face `(:inherit ,face :foreground "white"))
	;;  (propertize mode-line-powerline-separator-right 'face `(:inherit ,face :inverse-video t))))))))

  (advice-add 'evil-generate-mode-line-tag :around 'my/evil-generate-mode-line-tag))

(defface mode-line-evil-state
  `((t (:inherit mode-line :foreground ,(face-attribute 'mode-line :background))))
  "Meta-face used for property inheritance on all mode-line-evil-state faces."
  :group 'mode-line-evil-state)

(defface mode-line-evil-state-insert
  '((t (:inherit mode-line-evil-state :background "green")))
  "Face used in evil color-coded segments when in Insert state."
  :group 'mode-line-evil-state)

(defface mode-line-evil-state-normal
  '((t (:inherit mode-line-evil-state :background "red")))
  "Face used in evil color-coded segments when in Normal state."
  :group 'mode-line-evil-state)

(defface mode-line-evil-state-visual
  '((t (:inherit mode-line-evil-state :background "orange")))
  "Face used in evil color-coded segments when in Visual{,-Block,-Line} state."
  :group 'mode-line-evil-state)

(defface mode-line-evil-state-replace
  '((t (:inherit mode-line-evil-state :background "black")))
  "Face used in evil color-coded segments when in Replace state."
  :group 'mode-line-evil-state)

(defface mode-line-evil-state-motion
  '((t (:inherit mode-line-evil-state :background "blue")))
  "Face used in evil color-coded segments when in Motion state."
  :group 'mode-line-evil-state)

(defface mode-line-evil-state-operator
  '((t (:inherit mode-line-evil-state :background "violet")))
  "Face used in evil color-coded segments when in Operator state."
  :group 'mode-line-evil-state)

(defface mode-line-evil-state-emacs
  '((t (:inherit mode-line-evil-state :background "dark violet")))
  "Face used in evil color-coded segments when in Emacs state."
  :group 'mode-line-evil-state)

;;; End evil state


(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-hud t)
  (doom-modeline-env-version nil)
  (doom-modeline-workspace-name nil)
  :init (doom-modeline-mode 1))

(if t
    t

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
    '((t (:foreground "white" :inherit (ansi-color-bright-red mode-line))))
    "Flycheck warning face"
    :group 'mode-line-flycheck)

  (defface mode-line-flycheck-info
    '((t (:foreground "white" :inherit (ansi-color-yellow mode-line))))
    "Flycheck info face"
    :group 'mode-line-flycheck)

  (defface mode-line-flycheck-ok
    '((t (:foreground "white" :inherit (ansi-color-green mode-line))))
    "Flycheck ok face"
    :group 'mode-line-flycheck)

  (defface mode-line-flycheck-running
    '((t (:foreground "white" :inherit (ansi-color-blue mode-line))))
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
      (:propertize ,(concat " " text " ") face ,face)
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
	          (flycheck-enabled-checkers (my/mode-line-flycheck-segment 'ok))
	          (t ""))))

  (defvar mode-line-guard-status 'ok "The current guard status for mode-line.")

  (defun my/mode-line-guard-status ()
    "Full guard mode line string."
    (if (eq mode-line-guard-status 'ok)
        `("" (:propertize " \u2694 " face ,(my/mode-line-face 'accent)))
      `("" (:propertize " \u2694 " face ,(my/mode-line-flycheck-face 'error)))
      ))

  (defun my/mode-line-prettier-status ()
    "Mode line prettier indicator."
    (if prettier-js-mode
        `("" (:propertize " \uf789 " face ,(my/mode-line-face 'accent)))
      `("")
      ))

  (defun my/mode-name ()
    (let ((mode (pcase major-mode
	                ('magit-status-mode "\ue725")
	                ('ruby-mode "\ue21e") ;; "\uf48e") ;; \U0001f48e
	                ('web-mode "\ue60e")
	                ('rjsx-mode "\ue7ba")
	                ('emacs-lisp-mode "(\ue779)")
	                (_ mode-name))))
      `(:propertize ,(concat " " mode " ") face ,(my/mode-line-face 'accent))))

  (defvar-local my--projectile-project-p 'unset)
  (defun my/projectile-project-p ()
    "Cached version of projectile-project-p."
    (if (eq my--projectile-project-p 'unset)
        (setq my--projectile-project-p (projectile-project-p))
      my--projectile-project-p))
  
  (defvar-local my--projectile-project-name 'unset)
  (defun my/projectile-project-name ()
    "Cached version of projectile-project-name."
    (if (eq my--projectile-project-name 'unset)
        (setq my--projectile-project-name (projectile-project-name))
      my--projectile-project-name))

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
                                              ("" (:propertize (" " (:eval (my/mode-line-evil-tag)) " ") face ,(my/mode-line-face 'evil)))
				                                      ;; ,(if (frame-parameter nil 'client)
				                                      ;; 	   " @ "
				                                      ;; 	 )
				                                      ,(if (buffer-modified-p) " \u25CB " " \u25CF ")
				                                      ,(if (my/projectile-project-p)
					                                         `(""
					                                           (:propertize
					                                            (" " (:eval (my/projectile-project-name)) " ")
					                                            face
					                                            ,(my/mode-line-face 'accent)
					                                            )))
				                                      " "
                                              mode-line-buffer-identification
                                              ))

                          ;; right
                          (format-mode-line `("%e"
                                              ,(my/mode-line-prettier-status)
                                              ;;,(my/mode-line-guard-status)
                                              ,(my/mode-line-flycheck-status)
				                                      ,(my/mode-name)
                                              ))
                          ))))
  ;; (force-mode-line-update))
  )


(provide 'dm-mode-line)

;;; dm-mode-line.el ends here
