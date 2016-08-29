;; base16-firewatch-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Code:

(require 'base16-theme)

(let ((syntax-hue 220)
      (syntax-saturation 0.13)
      (syntax-brightness 0.18))
  (let ((mono-1 (my/hsl-to-hex syntax-hue 0.14 0.71))
        (mono-2 (my/hsl-to-hex syntax-hue 0.09 0.55))
        (mono-3 (my/hsl-to-hex syntax-hue 0.1 0.4))
        (hue-1  (my/hsl-to-hex 187 0.47 0.55))
        (hue-2  (my/hsl-to-hex 240 0.99 0.96))
        (hue-3  (my/hsl-to-hex 20 0.72 0.52))
        (hue-4  (my/hsl-to-hex 24 0.28 0.70))
        (hue-5  (my/hsl-to-hex 355 0.65 0.65))
        (hue-52 (my/hsl-to-hex 5 0.48 0.51))
        (hue-6  (my/hsl-to-hex 29 0.54 0.61))
        (hue-62 (my/hsl-to-hex 39 0.67 0.69)))
    (setq base16-firewatch-colors
      `(:base00 ,(my/hsl-to-hex syntax-hue syntax-saturation syntax-brightness)
        :base01 ,(my/hsl-to-hex syntax-hue syntax-saturation (- syntax-brightness 0.03))
        :base02 ,(my/hsl-to-hex syntax-hue syntax-saturation (- syntax-brightness 0.03))
        :base03 ,mono-3
        :base04 ,mono-2
        :base05 ,mono-1
        :base06 ,(my/hsl-to-hex 220 0.10 0.6)
        :base07 "#fdf4c1"
        :base08 ,hue-5
        :base09 ,hue-52
        :base0A ,hue-62
        :base0B ,hue-4
        :base0C ,hue-1
        :base0D ,hue-1
        :base0E ,hue-3
        :base0F ,(my/hsl-to-hex 220 1.0 0.66))
      )
  ))

;; Define the theme
(deftheme base16-firewatch)

;; Add all the faces to the theme
(base16-theme-define 'base16-firewatch base16-firewatch-colors)

;; Mark the theme as provided
(provide-theme 'base16-firewatch)

(provide 'base16-firewatch-theme)

;;; base16-firewatch-theme.el ends here
