;; base16-gruvbox-theme.el -- A base16 colorscheme

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Code:

(require 'base16-theme)

(defvar base16-gruvbox-colors
  '(:base00 "#282828"
    :base01 "#3c3836"
    :base02 "#504945"
    :base03 "#665c54"
    :base04 "#928374"
    :base05 "#bdae93"
    :base06 "#ebdbb2"
    :base07 "#fdf4c1"
    :base08 "#fb4934"
    :base09 "#fe8019"
    :base0A "#fabd2f"
    :base0B "#b8bb26"
    :base0C "#8ec07c"
    :base0D "#83a598"
    :base0E "#d3869b"
    :base0F "#8f3f71")
  "All colors for Base16 Gruvbox are defined here.")

;; Define the theme
(deftheme base16-gruvbox)

;; Add all the faces to the theme
(base16-theme-define 'base16-gruvbox base16-gruvbox-colors)

;; Mark the theme as provided
(provide-theme 'base16-gruvbox)

(provide 'base16-gruvbox-theme)

;;; base16-gruvbox-theme.el ends here
