;; base16-twilight-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Code:

(require 'base16-theme)


(defvar base16-twilight-colors
  '(:base00 "#141414"
    :base01 "#3c3836"
    :base02 "#504945"
    :base03 "#5F5A60"
    :base04 "#928374"
    :base05 "#F8F8F8"
    :base06 "#F0F0F0"
    :base07 "#FCFCFC"
    :base08 "#D2A8A1"
    :base09 "#9B703F"
    :base0A "#fabd2f"
    :base0B "#8F9D6A"
    :base0C "#7587A6"
    :base0D "#83a598"
    :base0E "#CDA869"
    :base0F "#8f3f71")
  "All colors for Base16 Twilight are defined here.")

;; Define the theme
(deftheme base16-twilight)

;; Add all the faces to the theme
(base16-theme-define 'base16-twilight base16-twilight-colors)

;; Mark the theme as provided
(provide-theme 'base16-twilight)

(provide 'base16-twilight-theme)

;;; base16-twilight-theme.el ends here
