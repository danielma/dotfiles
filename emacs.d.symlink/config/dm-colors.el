(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/Code/test/base16-builder-php/templates/emacs/build")

;; (use-packge dimmer)

(defun my/base16-set-theme (theme)
  "Set a base16 THEME by unloading all others."
  (interactive
   (list
    (completing-read "Load custom theme: "
                     (--map (s-chop-prefix "base16-" it)
                            (--filter (string-prefix-p "base16-" it)
                                      (mapcar
                                       'symbol-name
                                       (custom-available-themes)))))
    )
   )

  (--each
      (--filter (string-prefix-p "base16-" it) (mapcar 'symbol-name custom-enabled-themes))
    (disable-theme (intern it)))
  (load-theme (intern (concat "base16-" theme)) t)
  ;; (enable-theme 'session-face)
  )

(defun my/set-custom-face (font)
  "Interactively set the FONT for the custom theme."
  (interactive
   (let* ((completion-ignore-case t)
          (font (completing-read "Font name: "
                                 ;; x-list-fonts will fail with an error
                                 ;; if this frame doesn't support fonts.
                                 (x-list-fonts "*-normal-normal-*" nil (selected-frame))
                                 nil nil nil nil
                                 (frame-parameter nil 'font))))
     (list font)))
  (custom-theme-set-faces
   'session-face
   `(default ((t (:weight normal :width normal :slant normal :font ,font))))))


(let* ((themename (getenv "BASE16_THEME"))
       (theme (intern (concat "base16-" themename))))
  (load-theme (if (member theme (custom-available-themes))
		  theme
		'base16-default-dark) t))

;; (deftheme session-face)

;; (custom-theme-set-faces
;;  'session-face
;;  '(default ((t (:weight normal :height 120 :width normal :family "IBM Plex Mono")))))

;; (provide-theme 'session-face)

;; (enable-theme 'session-face)

(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(set-frame-parameter (selected-frame) 'alpha 95)

(use-package whitespace
  :init
  (setq whitespace-line-column 100
        whitespace-style '(face lines-tail))
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode))

(provide 'dm-colors)
