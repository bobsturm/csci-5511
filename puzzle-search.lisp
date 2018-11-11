(defstruct location
  i  ;; number representing row index
  j  ;; number representing column index
)

(defun create-state (0_0 0_1 0_2 1_0 1_1 1_2 2_0 2_1 2_2)
;   (make-array '(3 3) :initial-contents (list (list 0_0 0_1 0_2) (list 1_0 1_1 1_2) (list 2_0 2_1 2_2)))
  (list (list 0_0 0_1 0_2) (list 1_0 1_1 1_2) (list 2_0 2_1 2_2))
)

(defparameter *blank-square* -99 "constant representing the blank square")
(defparameter *goal-state* (create-state 1 2 3 4 5 6 7 8 *blank-square*))

;; TODO: remove hardcoded refs to the board being 3x3
;; TODO: make board size refs constants or variables

;; todo: change state to be a list?

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

(defun get-loc-value(state i j)
 (nth j (nth i state))
)

(defun get-new-value (state loc swaploc1 swaploc2)
   (let ((swap1i (location-i swaploc1))
         (swap1j (location-j swaploc1))
         (swap2i (location-i swaploc2))
         (swap2j (location-j swaploc2))
         (curi (location-i loc))
         (curj (location-j loc)))
            (if (and (equal curi swap1i) (equal curj swap1j))
               (get-loc-value state swap2i swap2j)
                  (if (and (equal curi swap2i) (equal curj swap2j))
                     (get-loc-value state swap1i swap1j)
                        (get-loc-value state curi curj)
               )
         )
    )
)

;; returns a 2 element array representing i (row index) and j (column index) of the given tile.
;; caller should seed i and j to 0 and 0.
(defun find-loc-inner (state tile i j)
   (if (> j 2) (error "TILE NOT FOUND!!")) 
;   (if (equal (aref state i j) tile)
;      (make-location :i i :j j)
;      (if (< i 2)
;         (find-loc-inner state tile (+ i 1) j)
;         (find-loc-inner state tile 0 (+ j 1))
;      )
;   )
   (if (equal (get-loc-value state i j) tile)
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

; returns state if it is valid
; raises error if state is invalid
; TODO: use row-major-aref and array map to validate that numbers are unique?
(defun validate-state (state)
  state
)


;; newloc is a location structure
;; returns nil if the move is not legal
(defun move (state newloc)
  (let ((loc (find-loc state *blank-square*)))
;     (format t "loc:~S~%" loc)
     (if (is-legal-move loc newloc)
;        (let ((curval (aref state (location-i newloc) (location-j newloc)))
;              (newstate (create-state (aref state 0 0) (aref state 0 1) (aref state 0 2) (aref state 1 0) (aref state 1 1) (aref state 1 2) (aref state 2 0) (aref state 2 1) (aref state 2 2))))
;           (setf (aref newstate (location-i newloc) (location-j newloc)) *blank-square*)
;           (setf (aref newstate (location-i loc) (location-j loc)) curval)
;           (validate-state newstate)
;        )
         (progn
           (format t "~%past is-legal-move. loc:~S newloc:~S~%" loc newloc)
         (create-state (get-new-value state (make-location :i 0 :j 0) newloc loc)
                       (get-new-value state (make-location :i 0 :j 1) newloc loc)
                       (get-new-value state (make-location :i 0 :j 2) newloc loc)
                       (get-new-value state (make-location :i 1 :j 0) newloc loc)
                       (get-new-value state (make-location :i 1 :j 1) newloc loc)
                       (get-new-value state (make-location :i 1 :j 2) newloc loc)
                       (get-new-value state (make-location :i 2 :j 0) newloc loc)
                       (get-new-value state (make-location :i 2 :j 1) newloc loc)
                       (get-new-value state (make-location :i 2 :j 2) newloc loc))
     ))
   )  
)

(defun generate-child-states (parentstate)
       (let ((blankloc (find-loc parentstate *blank-square*)))
         (remove nil (list (move parentstate (make-location :i (- (location-i blankloc) 1) :j (location-j blankloc))) ;; move up
                           (move parentstate (make-location :i (+ (location-i blankloc) 1) :j (location-j blankloc))) ;; move down
                           (move parentstate (make-location :i (location-i blankloc) :j (+ (location-j blankloc) 1))) ;; move right
                           (move parentstate (make-location :i (location-i blankloc) :j (- (location-j blankloc) 1))) ;; move left
                      )
         )
       )
)


(defun puzzle-search ()
;; create the start state and put it in the frontier
   (let ((root (create-root)))
      (a-star-search root #'generate-child-states)
   )
)


