; prints a single node
(defun print-node (n summary)
   (if summary
      (format t "---~%NAME:~S~%\STATE:~S~%h(n):~S~%NUM-CHILDREN:~S~%" (search-node-name n) (search-node-state n) (search-node-h_n n) (list-length (search-node-children n)))
;         (format t "~S~%" n)
   )
)

;; breadth first print.
(defun print-tree (n summary)
   (if (not (null n))
       (if (listp n)
          (progn 
             (print-node (car n) summary)
             (print-tree (cdr n) summary)
             (print-tree (search-node-children (car n)) summary )
          )
            (progn
               (print-node n summary)
               (print-tree (search-node-children n) summary)
            )
        )
   )

)