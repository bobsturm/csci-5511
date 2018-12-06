;;;; A-STAR Search Package
;;;;
;;;; This package implements the A-STAR Search.  
;;;;
;;;; The consumer of this package determines the structure for state.  The states stored and used for a-star
;;;; are opaque.  i.e. This package has no knowledge or requirements for how state is represented.
;;;; Rather, the consumer must provide functions that generates child states and calculates heuristics that are dependent
;;;; knowledge of the state structure.
;;;;
;;;; There are only 2 functions that should be considered public functions:  a-star-search and print-tree.
;;;; The consumer of these functions is expected to provide:
;;;;   1) A heuristic function that, given a current state and a candidate future state, returns a heuristic cost for the potential state transition.
;;;;   2) A child state generation function that, given a current state, generates all candidate future states that might be chosen as the next state.;
;;;;   3) A cost function that, given the current cost, a current state, and a future state, calculates the total cost of moving from current state to future state.
;;;;   4) A state equality function that, given 2 states returns t if those states are equivalent.
;;;;   5) A print helper function that, given a state, produces a pretty print version of that state.
;;;;
;;;; This package handles unsolveable searches in the following manner.  a-star-search will return the tree from the search and there will not be a leaf node with h(n) value of 0.
;;;; This can be observed by calling print-tree and inspecting the results.  If a cycle was detected and hence the tree is unsolvable, The following string will be printed 
;;;; ***THIS STATE IS A CYCLE. NO SOLUTION POSSIBLE.***
;;;; in the node that contains a repeated state.

(in-package :cl-user)

;;; All functions after print-tree are exposed solely for testing purposes
;;; Consumers shoul only call a-star-search and print-tree.
(defpackage a-star
   (:export :a-star-search :print-tree :test-all-a-star-search :create-node :create-child-nodes :set-children :set-g_n :is-cycle :get-children :tree-to-list)
)

(in-package :a-star)

;;; structure for nodes within the search tree
;;; private to this package!
;;;
;;; IMPORTANT NOTE: Initially, parent was a pointer to the parent node.  This was changed to be a string representing the parent node nbame due to issues with cycles.
;;;                 Lisp likes to print out objects and the default print for 2 objects that have references to each other (parent and children properties) create stack overflows from an infinite loop!!!
;;;                 Disappointing! This results in slower performance when wanting to find a parent becuase a search of the tree has to be done.
;;;
(defstruct search-node 
   name      ; The name of this node.  Needs to be unique.  This is used to find a parent node.
   state     ; Object representing the state of the world.  This is controlled by the consumer and is opaque to the a-star package.
   parent    ; The name of the parent for this node.  BEWARE!!!!  SEE THE IMPORTANT NOTE ABOVE.
   h_n       ; A number representing the heuristic function's value for the cost of moving to this node from its parent node's state.
   g_n       ; A number representing the actual cost of the path to this node.  this is nil until this node has been expanded.
   f_n       ; cost to parent + h_n.  Although this is a derivable value, we put it in here to make sorting the frontier easy.
   children  ; list of nodes created for potential expansion.  nil if this node has not yet been expanded.
   is-repeat ; flag indicating if the state is a repeat state and the search was aborted.
)


;;; Prints out a single search-node from the search tree.  Requires a function that encapsulates the printing of the state
;;; since the state is opque to a-star.
;;; All nodes that do not have a g(n) value are filtered because this signals that the node was not expanded.  If detail is passe in as t
;;; all nodes are printed, regardless of whether or not they have a g(n) value (not nil).
(defun print-node (n detail fn-get-printable-state)
   (let ((gn  (search-node-g_n n)))
;(format t "INSIDE PRINT=NODE - g_n ~S~%" gn)
        (if (or (not (null gn)) detail)  ; a value for g(n) indicates the node was visited and has a cost.
            (progn
               (format t "~&--------~%~S" (search-node-name n))
               (format t "~&~S" (funcall fn-get-printable-state  (search-node-state n)))
               (format t "~&h(n):~S" (search-node-h_n n))
               (if (equalp (search-node-h_n n) 0)
                   (format t "  ***GOAL STATE***")
               )
               (if (null gn)
                   (format t "~&***NODE WAS GENERATED, BUT NOT EXPANDED***")
               )
               (format t "~&g(n):~S~&f(n):~S~%NUM-CHILDREN:~S~%" gn (search-node-f_n n)  (list-length (search-node-children n)))
               (if (search-node-is-repeat n)
                   (format t "~&***THIS STATE IS A CYCLE. NO SOLUTION POSSIBLE.***")
               )
            )
         )
    )
)

;;; Prints the given tree.  Prints  nodes that have not been expanded when detail is t.  Otherwise, only 
;;; expanded nodes are printed.  The tree is printed in breadth first order.
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

;;; Utility function to set children on a search-node
(defun set-children (node children)
  (setf (search-node-children node) children)
  node
)

;;; Utility function to get the children of a search-node
(defun get-children (node)
  (search-node-children node)
)

;;; Utility function to get the state of a search-node.
(defun get-state (node)
   (if (not (null node))
      (search-node-state node)
   )
)

;;; Utility function to set the actual cost on a node.
(defun set-g_n (node g_n)
   (setf (search-node-g_n node) g_n)
   node
)

;;; This function creates a search-node.  f_n is derived by adding the h_n value with the g_n value from the parent
;;; provided.  Parent is set to the name of the parent (SEE NOTE ABOVE REGARDING CYCLES AND equalp and printing).
;;; If parent is nil, it is assumed that this is the root node and f_n is set to h_n.
(defun create-node (parent name state h_n)
   (let ((f_n 
            (if (null parent) h_n
               (if (null (search-node-g_n parent)) h_n
                  (+ (search-node-g_n parent) h_n)
               )
            )
         ))
      (make-search-node 
          :name (if (null parent) name 
                  (concatenate 'string (search-node-name parent) (write-to-string '-) name)
                )
          :state state
          :h_n h_n
          :parent (if (null parent) nil (search-node-name parent))
;TODO: THIS IS A TEMP HACK.  NEED TO CHANG
          :f_n (if (not (null f_n)) (+ f_n 1) f_n)
      )
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

(defun search-node-sorter (a b)
   (< (search-node-h_n a) (search-node-h_n b))
)

;;; Returns t if the state on the nodes are equal, the nodes do not have the same name
;;; and the nodes have both been popped from the frontier (and thus g_n is valued).
(defun is-duplicate-state (node1 node2 fn-state-equal)
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
  (not (null (find-if #'(lambda (x) (is-duplicate-state x newnode fn-state-equal)) (tree-to-list tree))))
)

;;; Utility function to flatten the given tree into a list.
(defun tree-to-list (tree)
  (if (not (null tree))
     (if (listp tree)
        (append (list (car tree)) (tree-to-list (cdr tree)) (tree-to-list (search-node-children (car tree))))
      (append (list tree) (tree-to-list (search-node-children tree)))
     )
  )
)

;;; Returns nil if parent is not found
(defun find-parent (tree node)
   (find-if #'(lambda (x) (equalp (search-node-name x) (search-node-parent node))) (tree-to-list tree))
)

;;; This function returns the actual cost given a parent and the provided g-cost function.
;;; The cost is the sum of the value returned from the g-cost function and the g(n) value from the parent
;;; provided.  If parent is nil, it assumed that the cost calculation is for the root node
;;; and 0 is returned.
(defun calc-cost (parent node fn-g-cost)
   (if (null node)
      (error "node must not be nil to calculate cost!")
   )
   (if (not (null parent))
      (funcall fn-g-cost (search-node-g_n parent) (get-state parent) (get-state node))
     0
   )
)

;;; Returns t if the given node state is equal to teh given goal-state as determined
;;; by the function provided.
(defun is-goal-state (goal-state node fn-state-equal)
   (if (or (null goal-state) (null node))
      (error "goal-state and node must not be nil")
   )
   (funcall fn-state-equal goal-state (search-node-state node))
)

;; input:  the frontier
;; assumes thqt the frontier is sorted by f(n) = h(n) + g(n).

;;; This function is the heart of a-star search.  It recursively pops nodes from the fontier until either a cycle (meaning that the
;;; search is not solvable) or a solution is found.
;;; Requires functions to create child states, calculate heuristic, calculate g-cost, and compare states.  These required
;;; functions are specific to the problem and thus are not provided by a-star.
(defun expand-next (root frontier goal-state curcost curnode fn-generate-child-states fn-heuristic fn-g-cost fn-state-equal)
   (if (not (equal curcost 10000))
(progn
   (if (or (null frontier) (not (listp frontier)))
      (error "Frontier is either nil or not a list!  frontier:~S~%" frontier)
   )
   (let ((next (pop frontier)))
      (let ((parent (find-parent root next)))         
         (set-g_n next (calc-cost parent next fn-g-cost)) ; sets the actual cost and marks the node as expanded.
        (format t "set g_n to be ~S" (search-node-g_n next))
         (if (not (is-cycle root next fn-state-equal)) ; end the search before expanding this node.  caller will know search failed because h_x is not 0
            (if (not (is-goal-state goal-state next fn-state-equal)) ; stop if we hit the goal state.
               (let ((children (create-child-nodes next fn-generate-child-states fn-heuristic))) ; where can we go from here?
                  (set-children next children) ; connect the generated nodes to their parent.
                  (expand-next ; keep going. Note that the frontier is sorted by f_n
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
         (setf (search-node-is-repeat next) t) ; set the repeat flag and abort the search becuase a cycle was detected.
      )
     )
   )
)
)
)


;; root: the root of the search tree to search
(defun a-star:a-star-search (start-state goal-state fn-generate-child-states fn-heuristic fn-g-cost fn-state-equal fn-get-printable-state)
   (format t "~&STARTING A* SEARCH~%")
   (let ((root (create-node nil "ROOT" start-state (funcall fn-heuristic nil start-state))))
      ;; start the search...
      (expand-next root (list root) goal-state 0 nil fn-generate-child-states fn-heuristic fn-g-cost fn-state-equal)
      (format t "~&A* SEARCH COMPLETED~%")
      root
   )
)
