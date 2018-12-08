;;;; functionatom
;;;;

(in-package :cl-user)

;;; This function takes a list of arbitrary structure and returns a flattened list.  Order of the atoms
;;; is maintained.
(defun functionatom (l)
  (if (not (null l))
      (if (listp (car l)) 
          (functionatom (append (functionatom (car l)) (functionatom (cdr l))))
        (cons (car l) (functionatom (cdr l))) ; else
      )
  )
)
