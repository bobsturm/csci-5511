;;;; replaceword

(in-package :cl-user)

;;; Utility functio to validate the input parameters to replaceword.
(defun check-parms (sym l)
   (if (or (not (symbolp sym)) (not (listp l))) (error "Invalid parms.  sym must be a symbol and l must be a list.")
   )
)

;;; Utility function for replaceword.
;;; This recursive function traverses through the given list l and replaces
;;; every occurrence of sym with the replacement symbol.
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

;;; Replaces in the list l all the instances of the symbol sym with 'YYYY
(defun replaceword (sym l)
   (check-parms sym l)
   (replaceword-inner sym l 'YYYY)
)
