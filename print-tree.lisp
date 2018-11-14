(defun print-state (s)
   (if (not (null s))
      (progn 
         (format t "~S~%" (car s))
         (print-state (cdr s))
      )
   )
)

; prints a single node
(defun print-node (n detail)
   (let ((child-count (list-length (search-node-children n)))
         (hn  (search-node-h_n n)))
            (if (or (> child-count 0) (equal hn 0) detail)
               (progn
                  (format t "---~%~S~%" (search-node-name n))
                  (print-state  (search-node-state n))
                  (format t "h(n):~S~%NUM-CHILDREN:~S~%" hn child-count)
               )
            )
    )
)

;; breadth first print.
(defun print-tree (n detail)
   (if (not (null n))
       (if (listp n)
          (progn 
             (print-node (car n) detail)
             (print-tree (cdr n) detail)
             (print-tree (search-node-children (car n)) detail)
          )
            (progn
               (print-node n detail)
               (print-tree (search-node-children n) detail)
            )
        )
   )

)