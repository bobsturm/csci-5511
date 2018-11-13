
;; structure for nodes within the search tree
(defstruct search-node 
   name      ;; engineer-friendly name for this node - used only for troubleshooting
   state     ;; object representing the state of the world
   parent    ;; pointer to thte parent search-node
   children  ;; list of nodes created for potential expansion.  nil if this node has not yet been expanded.
   h_n       ;; A number representing the heuristic function's value for this node
   g_n       ;; A number representing the actual cost of the path to this node.  this is nil until this node has been expanded.
)

(defun set-g_n (node g_n)
   (setq g_n (search-node-g_n node))
)

(defun set-h_n (node h_n)
   (setq h_n (search-node-h_n node))
)

(defun create-node (parent name state h_n)
   (make-search-node 
       :name (if (null parent) name 
               (concatenate 'string (search-node-name parent) (write-to-string '-\>) name)
             )
       :state state
       :h_n h_n
       :parent parent
   )
)

(defun create-child-nodes-inner (parent child-states fn-heuristic childnum)
  (if (not (null child-states))
      (cons 
          (create-node parent (write-to-string childnum) (car child-states) (funcall fn-heuristic (search-node-parent parent) (car child-states)))
          (create-child-nodes-inner parent (cdr child-states) fn-heuristic (+ childnum 1))
      )
   )
)

;; TODO: this needs tests!!
(defun create-child-nodes (parent fn-generate-child-states fn-heuristic)
   (let ((l (create-child-nodes-inner parent (funcall fn-generate-child-states (search-node-state parent)) fn-heuristic 1)))
           (format t "CREATED ~S children~%" (list-length l))
  l
)
)

;; TODO: Needs to also add in g(n) for A*
(defun search-node-sorter (a b)
   (< (search-node-h_n a) (search-node-h_n b))
)

;; input:  the frontier
;; assumes thqt the frontier is sorted by f(n) = h(n) + g(n).
(defun expand-next (frontier goal-state fn-generate-child-states fn-heuristic fn-state-equal stop)
;   (format t "PRINTING TREE:~%")
;   (print-tree frontier t)
   (if (not (equal stop 30))
(progn
   (if (or (null frontier) (not (listp frontier)))
      (error "Frontier is either nil or not a list!  frontier:~S~%" frontier)
   )
;   (format t "frontier size is:~S, stop:~S" (list-length frontier) stop)
   (let ((next (pop frontier)))
      (if (not (funcall fn-state-equal goal-state (search-node-state next)))
          (let ((children (create-child-nodes next fn-generate-child-states fn-heuristic)))
;             (format t "goal-state:~S next~S~%" goal-state next)
             (setf (search-node-children next) children) ;; set the children on their parent
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
(defun a-star-search (start-state goal-state fn-generate-child-states fn-heuristic fn-state-equal)
   (format t "STARTING A* SEARCH~%")
   (let ((root (create-node nil "ROOT" start-state nil)))
      (set-g_n root 0)
      ;; start the search...
      (expand-next (list root) goal-state fn-generate-child-states fn-heuristic fn-state-equal 0)
      (format t "CALLING PRINT-TREE*********~%")
      (print-tree root t)
      (format t "A* SEARCH COMPLETED~%")
   )
)
