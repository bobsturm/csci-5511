;;;; This file contains functions needed to invok a-star-search on a 3x3 puzzle.

(in-package :cl-user)

;;; A location on the board, specified by row (i) and column (j)
(defstruct location
  i  ;; number representing row index
  j  ;; number representing column index
)

(defun create-state (0_0 0_1 0_2 1_0 1_1 1_2 2_0 2_1 2_2)
  (list (list 0_0 0_1 0_2) (list 1_0 1_1 1_2) (list 2_0 2_1 2_2))
)

(defparameter *blank-square* 0 "constant representing the blank square")
(defparameter *goal-state* (create-state 1 2 3 4 5 6 7 8 *blank-square*))
(defparameter *default-start-state* (create-state *blank-square* 1 3 4 2 5 7 8 6))

;; TODO: remove hardcoded refs to the board being 3x3
;; TODO: make board size refs constants or variables

;; todo: change state to be a list?


;; gets the value of the tile (or blank) on the given coordinates
(defun get-loc-value(state i j)
 (nth j (nth i state))
)

;; given a state of the board and 2 locations to swap, this will return the appropriate value for the loc given.
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

;; returns a location structure representing the location of the given tile value.
;; caller should seed i and j to 0 and 0.
(defun find-loc-inner (state tile i j)
   (if (> j 2) (error "TILE NOT FOUND!!")) 

   (if (equal (get-loc-value state i j) tile)
      (make-location :i i :j j)
      (if (< i 2)
         (find-loc-inner state tile (+ i 1) j)
           (find-loc-inner state tile 0 (+ j 1))
      )
   )
)

;; finds the given tile in the state rpresentation
(defun find-loc (state tile)
   (find-loc-inner state tile 0 0)
)

(defun is-legal (i j)
  (and (< i 3) (> i -1) (< j 3) (> j -1))
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

;;; This print function is used by a-star to print the state of the puzzle.
;;; Given a state, this returns a pretty print representation of the state.
;;; For example:
;;;
;;; "(1 2 3)
;;;  (4 5 6)
;;;  (7 8 0)
;;; "
(defun get-printable-state (s)
   (if (not (null s))
      (let ((row1 (format nil "~S~%" (car s)))
            (remainder  (get-printable-state (cdr s))))
         (if (null remainder)
             row1
                 (concatenate 'string row1 " " remainder)
         )
      )
   )
)

;; newloc is a location structure
;; returns nil if the move is not legal
(defun move (state newloc)
  (let ((loc (find-loc state *blank-square*)))
     (if (is-legal-move loc newloc)
         (create-state (get-new-value state (make-location :i 0 :j 0) newloc loc)
                       (get-new-value state (make-location :i 0 :j 1) newloc loc)
                       (get-new-value state (make-location :i 0 :j 2) newloc loc)
                       (get-new-value state (make-location :i 1 :j 0) newloc loc)
                       (get-new-value state (make-location :i 1 :j 1) newloc loc)
                       (get-new-value state (make-location :i 1 :j 2) newloc loc)
                       (get-new-value state (make-location :i 2 :j 0) newloc loc)
                       (get-new-value state (make-location :i 2 :j 1) newloc loc)
                       (get-new-value state (make-location :i 2 :j 2) newloc loc))
     )
   )  
)

;;; Given a parent state, generates all legal 8-puzzle moves from this state.
;;; A list of the candidate child states is returned.
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

;;; Utility function for calculating manhattan distance for the (i, j) tile.
;;; If *blank-square* is located at the (i, j) location for from-state, 0 is returned.
(defun man-distance-filter-blank (i j from-state to-state)
   (if (equal (get-loc-value from-state i j) *blank-square*) 0
      (man-distance i j from-state to-state)
   )
)

;;; Given 2 states, calculates the manhattan distance for tile in the (i, j) position.
;;; i and j are coordinates to the tile in from-state to measure distance to to-state
(defun man-distance (i j from-state to-state)
   (let ((tile (get-loc-value from-state i j)))
      (let ((to-loc (find-loc to-state tile)))
         (+ (abs (- (location-i to-loc) i)) (abs (- (location-j to-loc) j)))
      )
   )
)

;;; Utility function for heuristic.  Sums up the man-distance between the 2 given states: cur and goal.
;;; The *blank-square* is not included in the sum.
(defun heuristic-inner (i j cur goal)
   (if (> j 2)  0
      (+ ; (man-distance-filter-blank i j cur start)
         (man-distance-filter-blank i j cur goal)
         (if (equal i 2) (heuristic-inner 0 (+ j 1) cur goal)
             (heuristic-inner (+ 1 i) j cur goal)
         )
      )
   )
)

;;; This is the heuristic function for 8-puzzle.  The signature for this method requires a current puzzle state and a future puzzle state.
;;; This is due to the requirements of a-star-search.  For a puzzle search, only futurestate is used in the calculation.
;;; This heuristic sums all the manhattan distances for each number from the *heuristic-goal-state* global variable.
;;; The blank square is not added to the sum - only numbers are considered.
(defun heuristic (curstate futurestate)
   (if (null *heuristic-goal-state*)
      (error "The global variable *heuristic-goal-state* must be setup before heuristics can be calculated!")
   )
   (heuristic-inner 0 0 futurestate *heuristic-goal-state*)
)

;;; Cost function for puzzle.
;;; This calculation is very simple.  Every state change is +1 from the previous cost, which is curcost.
;;; If nil is received as the prevstate, it is assumed that it is the initial state and hence a cost of 0
;;; is returned.
(defun g-cost (curcost prevstate newstate)
   (if (null prevstate)
       0 ; the root node is the only node with prevstate of nil
       (+ curcost 1)
   )
)

;;; Given 2 states, returns t if the states are equivalent; nil otherwise.
;;; For puzzle search, this is simply an equalp of the 2 lists.
;;; If nil is passed in for either state, and error is raised.
(defun state-equal (state1 state2)
   (if (not (and (listp state1) (listp state2)))
      (error "Unexpected type received within function ~S" #'state-equal)
         (equalp state1 state2)
   )
)

;; solves the 8-puzzle problem with the given start state of the board.
;; if no start state is provided the *default-start-state is used.
(defun puzzle-search (&key (start *default-start-state*) (print-detail nil))
   (if (null start)
       (setq start *default-start-state*)
   )
   (setq *heuristic-goal-state* *goal-state*)
   (let ((sol-root (a-star:a-star-search start *goal-state* #'generate-child-states #'heuristic #'g-cost #'state-equal #'get-printable-state)))
      (a-star::print-tree sol-root print-detail #'get-printable-state)
;      sol-root
   )

)