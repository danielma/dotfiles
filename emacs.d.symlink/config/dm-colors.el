(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/Code/test/base16-builder-php/templates/emacs/build")

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


(load-theme (if (member 'base16-onedark (custom-available-themes))
                'base16-onedark
              'base16-default-dark) t)

(deftheme session-face)

(custom-theme-set-faces
 'session-face
 '(default ((t (:weight normal :height 120 :width normal :family "Iosevka Nerd Font")))))

(provide-theme 'session-face)

(enable-theme 'session-face)

(set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend)

(provide 'dm-colors)
