(add-to-list 'custom-theme-load-path "~/.dotfiles/emacs/themes")
(add-to-list 'custom-theme-load-path "~/Code/test/base16-builder-php/templates/emacs/build")

;; (setq ns-use-srgb-colorspace nil)
(load-theme 'base16-onedark t)

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
  (enable-theme 'session-face)
  )

(defun my/set-custom-face (font)
  "Interactively set the FONT for the custom theme."
  (interactive
   (let* ((completion-ignore-case t)
          (font (completing-read "Font name: "
                                 ;; x-list-fonts will fail with an error
                                 ;; if this frame doesn't support fonts.
                                 (x-list-fonts "*" nil (selected-frame))
                                 nil nil nil nil
                                 (frame-parameter nil 'font))))
     (list font)))
  (custom-theme-set-faces
   'session-face
   `(default ((t (:weight normal :width normal :slant normal :font ,font))))))

(deftheme session-face)

(custom-theme-set-faces
 'session-face
 '(default ((t (:weight normal :height 120 :width normal :family "Input Mono Compressed")))))

(provide-theme 'session-face)

(enable-theme 'session-face)

(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)
 ;;; colors.el ends here
