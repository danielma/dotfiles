;;; dm-tabs --- My tabs configuration

;;; Commentary:

;;; Code:
(defun dm-tab-bar-name ()
  "Consistent width tab names for `tab-bar-mode`."
  (let ((tab-name (buffer-name (window-buffer (minibuffer-selected-window)))))
    (concat " " (s-truncate 20 (s-pad-right 20 " " tab-name)) " ")))

(use-package svg-lib)
(use-package svg-tag-mode)

(use-package tab-bar
  :after (s svg-lib svg-tag-mode)
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
  ;; (tab-bar-tab-name-format-function 'eli/tab-bar-tab-name-with-svg)
  :custom-face
  ;; (tab-bar     ((t :inherit default :box (:line-width 6))))
  (tab-bar ((t (:box (:line-width 4 :style flat-button)))))
  :config
  (tab-bar-mode))

(defface tab-bar-svg-active
  '((t (:inherit tab-line-tab)))
  ;; '((t (:foreground "#a1aeb5")))
  "Tab bar face for selected tab.")

(defface tab-bar-svg-inactive
  '((t (:inherit tab-line-tab-inactive)))
  ;; '((t (:foreground "#a1aeb5")))
  "Tab bar face for inactive tabs.")

(defun eli/tab-bar-svg-padding (width string)
  (let* ((style svg-lib-style-default)
         (margin      (plist-get style :margin))
         (txt-char-width  (window-font-width nil 'fixed-pitch))
         (tag-width (- width (* margin txt-char-width)))
         (padding (- (/ tag-width txt-char-width) (length string))))
    padding))

(defun eli/tab-bar-tab-name-with-svg (tab i)
  (let* ((current-p (eq (car tab) 'current-tab))
         (name (concat (if tab-bar-tab-hints (format "%d " i) "")
                       (alist-get 'name tab)
                       (or (and tab-bar-close-button-show
                                (not (eq tab-bar-close-button-show
                                         (if current-p 'non-selected 'selected)))
                                tab-bar-close-button)
                           "")))
         (padding (plist-get svg-lib-style-default :padding))
         (width)
         (image-scaling-factor 1.0))
    (when tab-bar-auto-width
      (setq width (/ (frame-inner-width)
                     (length (funcall tab-bar-tabs-function))))
      (when tab-bar-auto-width-min
        (setq width (max width (if (window-system)
                                   (nth 0 tab-bar-auto-width-min)
                                 (nth 1 tab-bar-auto-width-min)))))
      (when tab-bar-auto-width-max
        (setq width (min width (if (window-system)
                                   (nth 0 tab-bar-auto-width-max)
                                 (nth 1 tab-bar-auto-width-max)))))
      (setq padding (eli/tab-bar-svg-padding width name)))
    (propertize
     name
     'display
     (svg-tag-make
      name
      :face (if (eq (car tab) 'current-tab) 'tab-bar-svg-active 'tab-bar-svg-inactive)
      :inverse (eq (car tab) 'current-tab) :margin 0 :radius 6 :padding padding
      :height 1))))
;; :bind (:map global-map
;; 	 ("s-t" . tab-bar-new-tab)
;; 	 ("s-w" . tab-bar-close-tab)
;; 	 ("s-{" . tab-bar-switch-to-prev-tab)
;; 	 ("s-}" . tab-bar-switch-to-next-tab)))

(provide 'dm-tabs)

;;; dm-tabs.el ends here
