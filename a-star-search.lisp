
;; structure for nodes within the search tree
(defstruct search-node 
   name      ;; engineer-friendly name for this node - used only for troubleshooting
   state     ;; object representing the state of the world
   parent    ;; pointer to thte parent search-node
   children  ;; list of nodes created for potential expansion.  nil if this node has not yet been expanded.
   h_n       ;; A number representing the heuristic function's value for this node
   g_n       ;; A number representing the actual cost of the path to this node.  this is nil until this node has been expanded.
)
 

;; root: the root of the search tree to search
(defun a-star-search (root fn-generate-children)
   (print-node root)
   (let ((frontier (funcall fn-generate-children root)))
   )
)