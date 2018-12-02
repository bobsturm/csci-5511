;;;; replaceword extra credit.

(in-package :cl-user)

(defun check-parms (sym l)
   (if (or (not (symbolp sym)) (not (listp l))) (error "Invalid parms.  sym must be a symbol and l must be a list.")
   )
)

(defun replaceword-inner (sym l replace)
   (if (not (null l))
      (progn
         (if (not (symbolp (car l))) 
            (format t "WARNING: Found non-symbol within list:~S; this will be skipped.~%" (car l))
         )
         (if (equal (car l) sym) (cons replace (replaceword-inner sym (cdr l) replace))
            (cons (car l) (replaceword-inner sym (cdr l) replace))
         )
      )
   ) 
)

(defun replaceword (sym l)
   (check-parms sym l)
   (replaceword-inner sym l 'YYYY)
)
