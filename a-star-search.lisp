;;;; A-STAR Search Package

(in-package :cl-user)

;;; create-child-nodes and create-node are exported only for testing purposes.
(defpackage a-star
   (:export :a-star-search :create-child-nodes :create-node :set-children :print-tree)
)

(in-package :a-star)

;;; structure for nodes within the search tree
;;; private to this package
(defstruct search-node 
   name      ; engineer-friendly name for this node - used only for troubleshooting
   state     ; object representing the state of the world.  this is opaque to a-star
   parent    ; parent of this node - used for troubleshooting only   BEWARE!!!!  THIS WILL CAUSE LACK OF EQUALP CAPABILITY AND STACK OVERFLOWS ON FORMAT CALLS WITH ~S!!  THUS
             ; CYCLES HAVE BEEN REMOVED.  THIS WAS ORIGINALLY ADDED FOR DEBUGGING AND IS NOT NEEDED FOR A WORKING SOLUTION.
   children  ; list of nodes created for potential expansion.  nil if this node has not yet been expanded.
   h_n       ; A number representing the heuristic function's value for this node
   g_n       ; A number representing the actual cost of the path to this node.  this is nil until this node has been expanded.
)


; prints a single node
(defun print-node (n detail fn-get-printable-state)
   (let ((child-count (list-length (search-node-children n)))
         (hn  (search-node-h_n n)))
            (if (or (> child-count 0) (equal hn 0) detail)
               (progn
                  (format t "---~%~S~%" (search-node-name n))
                  (format t "~S~%" (funcall fn-get-printable-state  (search-node-state n)))
                  (format t "h(n):~S~%NUM-CHILDREN:~S~%" hn child-count)
               )
            )
    )
)

;; breadth first print.
(defun print-tree (n detail fn-get-printable-state)
   (if (not (null n))
       (if (listp n)   
          (progn 
             (print-node (car n) detail fn-get-printable-state)
             (a-star:print-tree (cdr n) detail fn-get-printable-state)
             (a-star:print-tree (search-node-children (car n)) detail fn-get-printable-state)
          )
            (progn      ;print the 
               (print-node n detail fn-get-printable-state)
               (a-star:print-tree (search-node-children n) detail fn-get-printable-state)
            )
        )
   )
)

(defun set-children (node children)
  (setf (search-node-children node) children)
)

;;todo: this doesn't work
(defun set-g_n (node g_n)
   (setq g_n (search-node-g_n node))
)

(defun create-node (parent name state h_n)
   (make-search-node 
       :name (if (null parent) name 
               (concatenate 'string (search-node-name parent) (write-to-string '-) name) ;(write-to-string '-\>)
             )
       :state state
       :h_n h_n
;       :parent parent
   )

)

;;; Utility function for create-child-nodes
(defun create-child-nodes-inner (parent child-states fn-heuristic childnum)
  (if (not (null child-states))
      (cons 
          (create-node parent (write-to-string childnum) (car child-states) (funcall fn-heuristic (search-node-parent parent) (car child-states)))
          (create-child-nodes-inner parent (cdr child-states) fn-heuristic (+ childnum 1))
      )
   )
)

;;; Given the parent search-node, function to create child states, and heuristic function, this function creates child nodes for parent
;;; and assigs them their corresponding heuristic value.  g_n is set to nil on the child nodes.  All child nodes are linked to their parent.
;;; A child node is created for every state returned from fn-generate-child-states.
(defun create-child-nodes (parent fn-generate-child-states fn-heuristic)
   (create-child-nodes-inner parent (funcall fn-generate-child-states (search-node-state parent)) fn-heuristic 1)
)

;; TODO: Needs to also add in g(n) for A*
(defun search-node-sorter (a b)
   (< (search-node-h_n a) (search-node-h_n b))
)

;; input:  the frontier
;; assumes thqt the frontier is sorted by f(n) = h(n) + g(n).
(defun expand-next (frontier goal-state fn-generate-child-states fn-heuristic fn-state-equal stop)
   (if (not (equal stop 30))
(progn
   (if (or (null frontier) (not (listp frontier)))
      (error "Frontier is either nil or not a list!  frontier:~S~%" frontier)
   )
   (let ((next (pop frontier)))
      (if (not (funcall fn-state-equal goal-state (search-node-state next)))
          (let ((children (create-child-nodes next fn-generate-child-states fn-heuristic)))
             (set-children next children)
             (expand-next 
                 (sort (append children frontier) #'search-node-sorter) 
                      goal-state
                      fn-generate-child-states
                      fn-heuristic
                      fn-state-equal (+ stop 1)) 
         )  
     )
   )
)
)
)

;; root: the root of the search tree to search
(defun a-star:a-star-search (start-state goal-state fn-generate-child-states fn-heuristic fn-state-equal fn-get-printable-state)
   (format t "STARTING A* SEARCH~%")
   (let ((root (a-star:create-node nil "ROOT" start-state (funcall fn-heuristic nil start-state))))
      (set-g_n root 0)
      ;; start the search...
      (expand-next (list root) goal-state fn-generate-child-states fn-heuristic fn-state-equal 0)
      (format t "A* SEARCH COMPLETED~%")
      root
   )
)
