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
;;;;   4) A print helper function that, given a state, produces a pretty print version of that state.
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
;;;        TODO: - After removing the cycle between parent and children, I found that CLisp allows the ability to deal with this via the *PRINT-CIRCLE* global var.
;;;              - This is very slow.  This should be profiled and refactored to improve response times.  There are many costly functions, such as find-parent.
;;;
(defstruct search-node 
   name      ; The name of this node.  Needs to be unique.  This is used to find a parent node.
   state     ; Object representing the state of the world.  This is controlled by the consumer and is opaque to the a-star package.
   parent    ; The name of the parent for this node.  BEWARE!!!!  SEE THE IMPORTANT NOTE ABOVE.
   h_n       ; A number representing the heuristic function's value for the cost of moving to this node from its parent node's state.
   g_n       ; A number representing the actual cost of the path to this node.  this is nil until this node has been expanded.
   children  ; list of nodes created for potential expansion.  nil if this node has not yet been expanded.
   is-repeat ; flag indicating if the state is a repeat state and the search was aborted.
)


;;; Utility function for determining whether a node has been expanded or not
(defun is-expanded (n)
   (> (list-length (search-node-children n)) 0)
)

;;; Utility function returns whether or not the goal state has been reached.
(defun is-goal (n)
  (equalp (search-node-h_n n) 0)
)

;;; Prints out a single search-node from the search tree.  Requires a function that encapsulates the printing of the state
;;; since the state is opque to a-star.
;;; All nodes that do not have a g(n) value are filtered because this signals that the node was not expanded.  If detail is passe in as t
;;; all nodes are printed, regardless of whether or not they have a g(n) value (not nil).
(defun print-node (n detail fn-get-printable-state)
    (if (or detail (equalp t (search-node-is-repeat n)) (is-expanded n) (is-goal n))
        (progn
           (format t "~&--------~%~S" (search-node-name n))
           (if (not (is-expanded n))
               (format t "~&NODE WAS GENERATED, BUT NOT EXPANDED")
           )
           (format t "~&~S" (funcall fn-get-printable-state  (search-node-state n)))
           (format t "~&h(n):~S" (search-node-h_n n))
           (if (equalp (search-node-h_n n) 0)
               (format t "  ***GOAL STATE***")
           )
           (format t "~&g(n):~S~&NUM-CHILDREN:~S~%" (search-node-g_n n) (list-length (search-node-children n)))
           (if (search-node-is-repeat n)
               (format t "~&***THIS STATE IS A CYCLE. NO SOLUTION POSSIBLE.***")
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
(defun create-node (parent name state g_n h_n)
   (if (not (numberp g_n))
      (error "g_n must be a number!")
   )
      (make-search-node 
          :name (if (null parent) name 
                  (concatenate 'string (search-node-name parent) (write-to-string '-) name)
                )
          :state state
          :h_n h_n
          :g_n g_n
          :parent (if (null parent) nil (search-node-name parent))
   )
)

;;; Utility function for create-child-nodes
(defun create-child-nodes-inner (parent child-states fn-g-cost fn-heuristic childnum)
  (if (not (null child-states))
      (let ((firststate (car child-states)) 
            (parentstate (search-node-state parent)))
         (cons 
             (create-node parent (write-to-string childnum) firststate 
                          (funcall fn-g-cost (search-node-g_n parent) parentstate firststate)  
                          (funcall fn-heuristic firststate))
             (create-child-nodes-inner parent (cdr child-states) fn-g-cost fn-heuristic (+ childnum 1))
         )
      )
   )
)

;;; Given the parent search-node, function to create child states, and heuristic function, this function creates child nodes for parent
;;; and assigs them their corresponding heuristic value.  g_n is set to nil on the child nodes.  All child nodes are linked to their parent.
;;; A child node is created for every state returned from fn-generate-child-states.
(defun create-child-nodes (parent fn-generate-child-states fn-g-cost fn-heuristic)
   (create-child-nodes-inner parent (funcall fn-generate-child-states (search-node-state parent)) fn-g-cost fn-heuristic 1)
)

(defun search-node-sorter (a b)
   (< (+ (search-node-g_n a) (search-node-h_n a)) 
      (+ (search-node-g_n b) (search-node-h_n b)))
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


;;; This function is the heart of a-star search.  It recursively pops nodes from the fontier until either a cycle (meaning that the
;;; search is not solvable) or a solution is found.
;;; Requires functions to create child states, calculate heuristic, calculate g-cost, and compare states.  These required
;;; functions are specific to the problem being solved and thus are not provided by a-star.
(defun expand-next (root frontier numiters maxiters curnode fn-generate-child-states fn-heuristic fn-g-cost)
   (if (or (null frontier) (not (listp frontier)))
      (error "Frontier is either nil or not a list!  frontier:~S~%" frontier)
   )
   (if (< numiters maxiters)
       (progn
          (let ((next (pop frontier)))
             (let ((parent (find-parent root next)))         
                 (if (not (equalp (search-node-h_n next) 0)) ; stop if we hit the goal state
                    (let ((children (create-child-nodes next fn-generate-child-states fn-g-cost fn-heuristic))) ; where can we go from here?
                       (set-children next children) ; connect the generated nodes to their parent.
                       (expand-next ; keep going. Note that the frontier is sorted by f_n
                        root
                        (sort (append children frontier) #'search-node-sorter) 
                        (+ numiters 1)
                        maxiters
                        next
                        fn-generate-child-states
                        fn-heuristic
                        fn-g-cost) 
                    )
                  (progn
                     (format t "GOAL STATE FOUND AFTER ~S NODE EXPANSIONS~%" numiters)
                     t ; puzzle solved!
                  )
              )
          )
        )
      ) ; if (< numiters maxiters)
    (format t "SEARCH ABORTED.!  ~S nodes were expanded, yet no solution was found.  The maximum iterations for the search was set to ~S~%" numiters maxiters)
   )
)


;;; The entry point to run a search.  See comments at the package header for use and what the required functions provide.
(defun a-star:a-star-search (maxiters start-state fn-generate-child-states fn-heuristic fn-g-cost fn-get-printable-state)
   (format t "~&STARTING A* SEARCH: Node expansion limit set to ~S~%" maxiters)
   (let ((root (create-node nil "ROOT" start-state 0 (funcall fn-heuristic start-state))))
      (let ((result (expand-next root (list root) 0 maxiters nil fn-generate-child-states fn-heuristic fn-g-cost)))
         (values root result)
      )
   )
)
