;;;; functionatom for extra credit 2.

(in-package :cl-user)

(defun functionatom (l)
  (if (not (null l))
      (if (listp (car l)) (functionatom (append (functionatom (car l)) (functionatom (cdr l))))
          (cons (car l) (functionatom (cdr l)))
      )
  )
)
