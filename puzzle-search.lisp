(defstruct location
  i  ;; number representing row index
  j  ;; number representing column index
)

(defun create-state (0_0 0_1 0_2 1_0 1_1 1_2 2_0 2_1 2_2)
   (make-array '(3 3) :initial-contents (list (list 0_0 0_1 0_2) (list 1_0 1_1 1_2) (list 2_0 2_1 2_2)))
)

(defparameter *blank-square* -99 "constant representing the blank square")
(defparameter *goal-state* (create-state 1 2 3 4 5 6 7 8 *blank-square*))

;; TODO: remove hardcoded refs to the board being 3x3
;; TODO: make board size refs constants or variables

(defun create-node (pname name state)
   (make-search-node 
       :name (if (null pname) (write-to-string name) 
               (concatenate 'string pname "." (write-to-string name))
             )
       :state state
   )
)

(defun create-root()
   (let ((root (create-node nil "0" (create-state *blank-square* 1 3 4 2 5 7 8 6))))
      (setf (search-node-g_n root) 0)
      root
   )
)

;; returns a 2 element array representing i (row index) and j (column index) of the given tile.
;; caller should seed i and j to 0 and 0.
(defun find-loc-inner (state tile i j)
   (if (> j 2) (error "TILE NOT FOUND!!")) 
   (if (equal (aref state i j) tile)
      (make-location :i i :j j)
      (if (< i 2)
         (find-loc-inner state tile (+ i 1) j)
         (find-loc-inner state tile 0 (+ j 1))
      )
   )
)
(defun find-loc (state tile)
   (find-loc-inner state tile 0 0)
)

(defun is-legal (i j)
  (and 
     (and (< i 3) (> i -1)) 
     (and (< j 3) (> j -1))
   )
)

; returns t if the proposed move is legal, nil otherwise
; the blank can move only 1 square and in only 1 direction
(defun is-legal-move (loc newloc)
  (let 
      ((i (location-i loc)) 
       (j (location-j loc))
       (newi (location-i newloc))
       (newj (location-j newloc)))
       (if (or (or (not (numberp i)) (not (numberp j))) (or (not (numberp newi)) (not (numberp newj))))
          (error "loc and newloc must both be of type location and contain 2 numbers, loc:~S, newloc~S" loc newloc)
       )
       (let ((disti (abs (- newi i)))
             (distj (abs (- newj j))))
;             (format t "i:~S, j:~S, newi:~S, newj:~S, disti:~S, distj:~S" i j newi newj disti distj)
             (if (not (is-legal i j)) 
                (error "Invalid loc provided ~S~%" loc)
             )
             (and 
                (is-legal newi newj)
                (or 
                   (and
                      (equal disti 1)
                      (equal distj 0))
                   (and
                      (equal 0 disti)
                      (equal 1 distj))
                 )
             )
      )
   )
)

;; newloc is the (i, j) list representing the new location
;; returns nil if the move is not legal
(defun move (state newloc)
;  (if (is-legal (first newloc) (second newloc)))
;     (let ((loc (find-loc(state *blank-square*))))
;     )
;  )      
)

;; Returns nil if the move is not legal.
(defun move-up (state)
;  (let ((loc (find-loc state *blank-square*)))
;      let ((newloc (list (first loc)
)

(defun generate-children (n)
;   (let (
;      (state (search-node-state n))
;      (children '())
;      (k 1))
;      (if (is-legal (+ 1 (find-loc state *blank-square*)))
;         (adjoin (create-child-node
;   )
)


(defun puzzle-search ()
;; create the start state and put it in the frontier
   (let ((root (create-root)))
      (a-star-search root #'generate-children)
   )
)


