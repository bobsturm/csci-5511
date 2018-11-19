;; style -> comments, design, indentation, debugging markers
;; !! LOTS OF COMMENTS 
;; identify infeasible puzzle


;; returns the given state as a pretty printable string
(defun get-printable-state (s)
   (if (not (null s))
      (let ((row1 (format nil "~S~%" (car s)))
            (remainder  (get-printable-state (cdr s))))
         (if (null remainder)
             row1
                 (concatenate 'string row1 " " remainder)
         )
      )
   )
)
