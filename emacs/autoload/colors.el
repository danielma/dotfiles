(add-to-list 'custom-theme-load-path "~/.dotfiles/emacs/themes")
(add-to-list 'custom-theme-load-path "~/Code/test/base16-builder-php/templates/emacs/build")

;; (setq ns-use-srgb-colorspace nil)
(load-theme 'base16-gruvbox t)

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
  )
