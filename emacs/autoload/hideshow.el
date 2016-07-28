(defun my-hideshow-setup ()
  (add-to-list 'hs-special-modes-alist
                 `(ruby-mode
                   ,(rx (or "def" "class" "module" "{" "[" "do")) ; Block start
                   ,(rx (or "}" "]" "end"))                  ; Block end
                   ,(rx (or "#" "=begin"))                   ; Comment start
                   ruby-forward-sexp nil))

  (add-function :before (symbol-function 'hs-find-block-beginning) #'end-of-line)
  )

(eval-after-load "hideshow" '(symbol-function 'my-hideshow-setup))
