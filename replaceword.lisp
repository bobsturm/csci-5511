(defun check-parms (sym l)
   (if (or (listp sym) (not (listp l))) (error "Invalid parms.  sym must be a symbol and l must be a list.")
   )
)

(defun replaceword (sym l)
   (progn
      (check-parms sym l)
      (if (not (null l)) (cons (car l) (cdr l))
      )
   )
)
