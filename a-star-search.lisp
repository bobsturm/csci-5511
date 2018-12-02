;;;; A-STAR Search Package

(in-package :cl-user)

;;; All functions after print-tree are exposed solely for testing purposes
(defpackage a-star
   (:export :a-star-search :print-tree :test-all-a-star-search :create-node :create-child-nodes :set-children :set-g_n :is-cycle :get-children)
)

(in-package :a-star)

;;; structure for nodes within the search tree
;;; private to this package
;;; IMPORTANT NOTE: Changed parent to be a string vs. an object due to issues with cycles.  Lisp likes to print out objects and the default print for 2 objects that have references to 
;;;   each other create stack overflows from an infinite loop.  Disappointing! This results in slower performance when wanting to find a parent becuase a search of the tree has to be done.
(defstruct search-node 
   name      ; engineer-friendly name for this node - used only for troubleshooting
   state     ; object representing the state of the world.  this is opaque to a-star
   parent    ; parent of this node - used for troubleshooting only   BEWARE!!!!  THIS WILL CAUSE LACK OF EQUALP CAPABILITY AND STACK OVERFLOWS ON FORMAT CALLS WITH ~S!!  THUS
             ; CYCLES HAVE BEEN REMOVED.  THIS WAS ORIGINALLY ADDED FOR DEBUGGING AND IS NOT NEEDED FOR A WORKING SOLUTION.
   h_n       ; A number representing the heuristic function's value for this node
   g_n       ; A number representing the actual cost of the path to this node.  this is nil until this node has been expanded.
   children  ; list of nodes created for potential expansion.  nil if this node has not yet been expanded.
)


; prints a single node
(defun print-node (n detail fn-get-printable-state)
   (let ((child-count (list-length (search-node-children n)))
         (gn  (search-node-g_n n)))
            (if (or (> child-count 0) (not (null gn)) detail)  ; a value for g(n) indicates the node was visited and has a cost.
               (progn
                  (format t "~&---~%~S" (search-node-name n))
                  (format t "~&~S" (funcall fn-get-printable-state  (search-node-state n)))
                  (format t "~&h(n):~S" (search-node-h_n n))
                  (if (equalp (search-node-h_n n) 0)
                      (format t "  ***GOAL STATE***")
                  )
                  (format t "~&g(n):~S~%NUM-CHILDREN:~S~%"  gn child-count)
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
            (progn 
               (print-node n detail fn-get-printable-state)
               (a-star:print-tree (search-node-children n) detail fn-get-printable-state)
            )
        )
   )
)

(defun set-children (node children)
  (setf (search-node-children node) children)
  node
)

(defun get-children (node)
  (search-node-children node)
)

(defun get-state (node)
   (if (not (null node))
      (search-node-state node)
   )
)

(defun set-g_n (node g_n)
   (setf (search-node-g_n node) g_n)
   node
)

(defun create-node (parent name state h_n)
   (make-search-node 
       :name (if (null parent) name 
               (concatenate 'string (search-node-name parent) (write-to-string '-) name) ;(write-to-string '-\>)
             )
       :state state
       :h_n h_n
       :parent (if (null parent) nil (search-node-name parent))
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

;; TODO: Needs to also add in g(n) of the parent node for true A*
(defun search-node-sorter (a b)
   (< (search-node-h_n a) (search-node-h_n b))
)

;;; Returns t if the state on the nodes are equal, the nodes do not have the same name
;;; and the nodes have both been popped from the frontier (and thus g_n is valued).
(defun is-duplicate-state (node1 node2 fn-state-equal)
   (format t "~&NODE1:~S~%NODE2:~S~%" node1 node2)
   (and (funcall fn-state-equal (get-state node1) (get-state node2)) 
        (not (equalp (search-node-name node1) (search-node-name node2)))
        (numberp (search-node-g_n node1))
        (numberp (search-node-g_n node2))
   )
)

;;; Returns t if a node with the same state as newnode is in teh tree.  Equivalence
;;; is tested using fn-state-equal.  All nodes with nil as g_n are not considered
;;; as a cycle because those nodes have not yet been officially used and therefore do not
;;; have a cost associated with them.  You may pass in a node that is connected to the tree
;;; for newnode.  For a cycle to exist, the 2 nodes must not have the same name. 
(defun is-cycle (tree newnode fn-state-equal)
   (if (not (null tree))
       (if (listp tree)
           (if (or (is-duplicate-state (car tree) newnode fn-state-equal)
                   (is-cycle (cdr tree) newnode fn-state-equal)
                   (is-cycle (search-node-children (car tree)) newnode fn-state-equal))
               t
           )
         (if (or (is-duplicate-state tree newnode fn-state-equal)
                 (is-cycle (search-node-children tree) newnode fn-state-equal))
             t
         )
       )
   )
)

;; input:  the frontier
;; assumes thqt the frontier is sorted by f(n) = h(n) + g(n).
(defun expand-next (root frontier goal-state curcost curnode fn-generate-child-states fn-heuristic fn-g-cost fn-state-equal)
   (if (not (equal curcost 100))
(progn
   (if (or (null frontier) (not (listp frontier)))
      (error "Frontier is either nil or not a list!  frontier:~S~%" frontier)
   )
   (let ((next (pop frontier)))
      (set-g_n next (funcall fn-g-cost curcost (get-state curnode) (get-state next)))
      (if (not (is-cycle root next fn-state-equal)) ; end the search before expanding this node.  caller will know search failed because h_x is not 0
         (if (not (funcall fn-state-equal goal-state (search-node-state next)))
            (let ((children (create-child-nodes next fn-generate-child-states fn-heuristic)))
               (set-children next children)
               (expand-next
                  root
                  (sort (append children frontier) #'search-node-sorter) 
                  goal-state
                  (search-node-g_n next)
                  next
                  fn-generate-child-states
                  fn-heuristic
                  fn-g-cost
                  fn-state-equal) 
         )  
     )
   )
  )
)
)
)


;; root: the root of the search tree to search
(defun a-star:a-star-search (start-state goal-state fn-generate-child-states fn-heuristic fn-g-cost fn-state-equal fn-get-printable-state)
   (format t "STARTING A* SEARCH~%")
   (let ((root (create-node nil "ROOT" start-state (funcall fn-heuristic nil start-state))))
      ;; start the search...
      (expand-next root (list root) goal-state 0 nil fn-generate-child-states fn-heuristic fn-g-cost fn-state-equal)
      (format t "A* SEARCH COMPLETED~%")
      root
   )
)
