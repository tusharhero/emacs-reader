;; .dir-locals.el
((c-mode . ((indent-tabs-mode . t)     ; Use spaces
            (c-basic-offset . 2)         ; 2-space indent
            (tab-width . 8)              ; Tab width (in case tabs are viewed)
            (c-default-style . "gnu")    ; Use GNU C style
            (fill-column . 80)))
 (emacs-lisp-mode . ((indent-tabs-mode . nil)
                     (lisp-indent-offset . 2)
                     (fill-column . 80)))
 (nil . ((require-final-newline . t)
         (show-trailing-whitespace . t))))
