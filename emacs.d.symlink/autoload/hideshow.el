(add-function :before (symbol-function 'hs-find-block-beginning) #'end-of-line)
