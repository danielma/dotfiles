(require 'speedbar)
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "Lucida Grande 11")
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))
