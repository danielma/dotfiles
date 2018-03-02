;; base16-panda-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Code:

(require 'base16-theme)

(defvar base16-panda-colors
  '(:base00 "#292A2B"
    :base01 "#3c3836"
    :base02 "#504945"
    :base03 "#665c54"
    :base04 "#676B79"
    :base05 "#E6E6E6"
    :base06 "#ebdbb2"
    :base07 "#ffffff"
    :base08 "#FF2C6D"
    :base09 "#FFB86C"
    :base0A "#ffcc95"
    :base0B "#19f9d8"
    :base0C "#6FC1FF"
    :base0D "#45A9F9"
    :base0E "#B084EB"
    :base0F "#FF75B5")
  "All colors for Base16 Panda are defined here.")

;; Define the theme
(deftheme base16-panda)

;; Add all the faces to the theme
(base16-theme-define 'base16-panda base16-panda-colors)

;; Mark the theme as provided
(provide-theme 'base16-panda)

(provide 'base16-panda-theme)

;;; base16-panda-theme.el ends here
