(progn
  (setq mode-line-format 
        '("%e" mode-line-front-space " " mode-line-client mode-line-modified mode-line-remote " " mode-line-buffer-identification mode-line-position evil-mode-line-tag
           mode-name mode-line-misc-info "  " mode-line-end-spaces))
  (force-mode-line-update))
;; (setq display-time-default-load-average nil)
;; (setq sml/no-confirm-load-theme t
;;       sml/position-percentage-format nil
;;       sml/mode-width 'full
;;       sml/name-width 40
;;       sml/theme 'respectful
;;       sml/shorten-modes t)
;; (add-to-list 'sml/replacer-regexp-list '("~/Code/\([^/]+\)" ":C:\1/") t)
;; (sml/setup)
(defun add-mode-line-dirtrack ()
    "When editing a file, show the last 2 directories of the current path in the mode line."
    (add-to-list 'mode-line-buffer-identification 
                 '(:eval (substring default-directory 
                                    (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))
(add-hook 'find-file-hook 'add-mode-line-dirtrack)
;; (telephone-line-defsegment tel-line-not-buffer
;;   '(""
;;     mode-line-mule-info
;;     mode-line-modified
;;     mode-line-client
;;     mode-line-remote
;;     mode-line-frame-identification)
;;   )

;; (telephone-line-defsegment tel-line-buffer
;;   (telephone-line-raw mode-line-buffer-identification t)
;;   )

;; (telephone-line-defsegment tel-line-projectile
;;   (if (projectile-project-p)
;;       (projectile-project-name)))

;; (setq telephone-line-lhs
;;         '((evil   . (mode-line-evil-tag-segment))
;;           (nil    . (telephone-line-erc-modified-channels-segment
;;                      telephone-line-process-segment
;;                      tel-line-not-buffer))
;;           (accent . (tel-line-projectile))
;;           (nil    . (tel-line-buffer))))
;; (setq telephone-line-rhs
;;         '((nil    . (telephone-line-misc-info-segment))
;;           (accent . (telephone-line-major-mode-segment))
;;           ))
;; (telephone-line-mode t)

;; write a function to do the spacing

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
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

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

(defun my/mode-line-flycheck-segment (kind)
  "Return propertized mode line segment for KIND."
  (if (flycheck-has-current-errors-p kind)
      `(""
        (:propertize
         ,(concat " " (number-to-string (my/flycheck-count-errors kind)) " ")
         face
         ,(my/mode-line-flycheck-face kind)
         ))
    ""))

(defun my/mode-line-flycheck-status ()
  "Full flycheck mode line string."
  (let ((errors (flycheck-count-errors flycheck-current-errors)))
    (cond (errors
           `(
             ,(my/mode-line-flycheck-segment 'error)
             ,(my/mode-line-flycheck-segment 'warning)
             ,(my/mode-line-flycheck-segment 'info)
             ))
          ((flycheck-running-p) '("" (:propertize " \u2219 " face mode-line-flycheck-running)))
          (t `("" (:propertize " \u2713 " face ,(my/mode-line-flycheck-face 'ok))))
          )))

;; use the function in conjunction with :eval and format-mode-line in your mode-line-format
;; (progn
  (setq-default mode-line-format
        '((:eval (simple-mode-line-render
                  ;; '("%e" mode-line-front-space " " mode-line-client mode-line-modified mode-line-remote " " mode-line-buffer-identification mode-line-position evil-mode-line-tag
                  ;; mode-name mode-line-misc-info "  " mode-line-end-spaces))
                  ;; left
                  (format-mode-line `(
                                      "%e"
                                      ("" (:propertize (" " (:eval (symbol-name evil-state)) " ") face ,(my/mode-line-face 'evil)))
                                      " "
                                      mode-line-client
                                      mode-line-modified
                                      " "
                                      ,(if (projectile-project-p)
                                          `(""
                                           (:propertize
                                            (" " (:eval (projectile-project-name)) " ")
                                            face
                                            ,(my/mode-line-face 'accent)
                                            )))
                                      " "
                                      mode-line-buffer-identification
                                      ))

                  ;; right
                  (format-mode-line `("%e"
                                      ,(my/mode-line-flycheck-status)
                                      " "
                                      mode-name
                                      " "
                                      ;; (""
                                      ;;  (:propertize
                                      ;;   (" " mode-name " ")
                                      ;;   face ,(my/mode-line-face 'accent)
                                      ;;   
                                      (""
                                       (:propertize
                                        (,(format-time-string " %I:%M "))
                                        face ,(my/mode-line-face 'accent)))
                                      " "
                                      ))
                  ))))
  ;; (force-mode-line-update))
 
