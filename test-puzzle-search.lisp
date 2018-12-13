;;;; This file contains tests for puzzle-search functions.  Call test-all-puzzle-search to test the puzzle search test suite.
;;;; These tests could be improved with more test cases on invlalid input to the functions.

(in-package :cl-user)

(defun test-is-legal()
  (progn (format t "Initiating tests for ~S...~%" #'is-legal) 
       (tf:error-if-failed2 0 0 T #'is-legal)
       (tf:error-if-failed2 0 2 T #'is-legal)
       (tf:error-if-failed2 2 0 T #'is-legal)
       (tf:error-if-failed2 2 2 T #'is-legal)
       (tf:error-if-failed2 0 -1 nil #'is-legal)
       (tf:error-if-failed2 -1 2 nil #'is-legal)
       (tf:error-if-failed2 1 3 nil #'is-legal)
       (tf:error-if-failed2 3 1 nil #'is-legal)
       (format t "~&All ~S tests completed.~%" #'is-legal)
    )
)

(defun test-find-loc()
  (progn (format t "Initiating tests for ~S...~%" #'find-loc) 
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 8 9) 1 (make-location :i 0 :j 0) #'find-loc)
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 8 9) 3 (make-location :i 0 :j 2) #'find-loc)
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 8 9) 4 (make-location :i 1 :j 0) #'find-loc)
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 8 9) 6 (make-location :i 1 :j 2) #'find-loc)
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 8 9) 7 (make-location :i 2 :j 0) #'find-loc)
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 -13 9) -13 (make-location :i 2 :j 1)#'find-loc)
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 8 9) 9 (make-location :i 2 :j 2) #'find-loc)
       (format t "~&All ~S tests completed.~%" #'find-loc)
    )
)

(defun test-is-legal-move()
  (progn (format t "Initiating tests for ~S...~%" #'is-legal-move) 
       ;; TODO: Add tests for invalid input
       (tf:error-if-failed2 (make-location :i 0 :j 0) (make-location :i 0 :j 1) t #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 0 :j 0) (make-location :i 1 :j 0) t #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 2 :j 2) (make-location :i 2 :j 1) t #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 1 :j 1) (make-location :i 2 :j 1) t #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 0 :j 0) (make-location :i 1 :j 1) nil #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 1 :j 1) (make-location :i 0 :j 0) nil #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 0 :j 0) (make-location :i 2 :j 0) nil #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 0 :j 2) (make-location :i 0 :j 2) nil #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 2 :j 0) (make-location :i 0 :j 0) nil #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 1 :j 1) (make-location :i 2 :j 0) nil #'is-legal-move)
       (tf:error-if-failed2 (make-location :i 0 :j 0) (make-location :i 2 :j 2) nil #'is-legal-move)
       (tf:assert-error-thrown () () #'is-legal-move)
       (tf:assert-error-thrown '(0 0) '(0 1) #'is-legal-move)
       (format t "~&All ~S tests completed.~%" #'is-legal-move)
    )
)

(defun test-move()
  (progn (format t "Initiating tests for ~S...~%" #'move) 
       ;; TODO: Add tests for invalid input
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 8 *blank-square*) (make-location :i 2 :j 1) (create-state 1 2 3 4 5 6 7 *blank-square* 8) #'move)
       (tf:error-if-failed2 (create-state 1 2 3 4 *blank-square* 6 7 8 5) (make-location :i 0 :j 1) (create-state 1 *blank-square* 3 4 2 6 7 8 5) #'move)
       (tf:error-if-failed2 (create-state 1 *blank-square* 2 3 4 5 6 7 8) (make-location :i 0 :j 0) (create-state *blank-square* 1 2 3 4 5 6 7 8) #'move)
       (tf:error-if-failed2 (create-state 1 2 *blank-square* 3 4 5 6 7 8) (make-location :i 1 :j 2) (create-state 1 2 5 3 4 *blank-square* 6 7 8) #'move)
       (tf:error-if-failed2 (create-state 1 *blank-square* 2 3 4 5 6 7 8) (make-location :i 1 :j 0) nil  #'move)
       (tf:error-if-failed2 (create-state 1 2 *blank-square* 3 4 5 6 7 8) (make-location :i 0 :j 0) nil  #'move)
       (tf:error-if-failed2 (create-state 1 2 3 4 *blank-square* 5 6 7 8) (make-location :i 2 :j 2) nil  #'move)
       (tf:error-if-failed2 (create-state 1 2 3 4 5 6 7 8 *blank-square*) (make-location :i 2 :j 3) nil  #'move)
       (tf:error-if-failed2 (create-state *blank-square* 1 2 3 4 5 6 7 8) (make-location :i -1 :j 0) nil  #'move)
       (format t "~&All ~S tests completed.~%" #'move)
    )
)

(defun test-generate-child-states()
  (progn (format t "Initiating tests for ~S...~%" #'generate-child-states) 
       ;; TODO: Add tests for invalid input
       (tf:error-if-failed1 (create-state 1 2 3 4 *blank-square* 5 6 7 8) (list (create-state 1 *blank-square* 3 4 2 5 6 7 8)
                                                                             (create-state 1 2 3 4 7 5 6 *blank-square* 8) 
                                                                             (create-state 1 2 3 4 5 *blank-square* 6 7 8)
                                                                             (create-state 1 2 3 *blank-square* 4 5 6 7 8)) #'generate-child-states)
       (tf:error-if-failed1 (create-state *blank-square* 8 7 6 5 4 3 2 1) (list (create-state 6 8 7 *blank-square* 5 4 3 2 1) 
                                                                             (create-state 8 *blank-square* 7 6 5 4 3 2 1)) #'generate-child-states)
       (tf:error-if-failed1 (create-state 8 *blank-square* 7 6 5 4 3 2 1) (list (create-state 8 5 7 6 *blank-square* 4 3 2 1) 
                                                                             (create-state 8 7 *blank-square* 6 5 4 3 2 1)
                                                                             (create-state *blank-square* 8 7 6 5 4 3 2 1)) #'generate-child-states)
       (tf:error-if-failed1 (create-state 8 7 *blank-square* 6 5 4 3 2 1) (list (create-state 8 7 4 6 5 *blank-square* 3 2 1) 
                                                                             (create-state 8 *blank-square* 7 6 5 4 3 2 1)) #'generate-child-states)
       (tf:error-if-failed1 (create-state 8 7 6 5 4 3 2 1 *blank-square*) (list (create-state 8 7 6 5 4 *blank-square* 2 1 3) 
                                                                             (create-state 8 7 6 5 4 3 2 *blank-square* 1)) #'generate-child-states)
       (tf:error-if-failed1 (create-state 8 7 6 *blank-square* 5 4 3 2 1) (list (create-state *blank-square* 7 6 8 5 4 3 2 1)
                                                                             (create-state 8 7 6 3 5 4 *blank-square* 2 1) 
                                                                             (create-state 8 7 6 5 *blank-square* 4 3 2 1)) #'generate-child-states)
       (format t "~&All ~S tests completed.~%" #'generate-child-states)
    )
)

(defun test-man-distance()
  (progn (format t "Initiating tests for ~S...~%" #'man-distance) 
       (tf:error-if-failed4 0 0 (create-state 1 2 3 4 5 6 7 8 9) (create-state 1 2 3 4 5 6 7 8 9) 0 #'man-distance)
       (tf:error-if-failed4 2 2 (create-state 1 2 3 4 5 6 7 8 9) (create-state 9 2 3 4 5 6 7 8 1) 4 #'man-distance)
       (tf:error-if-failed4 1 2 (create-state 1 2 3 4 5 6 7 8 9) (create-state 1 2 3 6 5 4 7 8 9) 2 #'man-distance)
       (tf:error-if-failed4 2 1 (create-state 1 2 3 4 5 6 7 8 9) (create-state 1 2 3 4 5 6 8 7 9) 1 #'man-distance)
       (format t "~&All ~S tests completed.~%" #'man-distance)
    )
)

(defun test-heuristic()
   (setq *heuristic-start-state* *default-start-state*)
   (setq *heuristic-goal-state* *goal-state*)

   (progn (format t "Initiating tests for ~S...~%" #'heuristic) 
       (tf:error-if-failed1 *goal-state* 0 #'heuristic) ; 4
       (tf:error-if-failed1 (create-state 1 2 *blank-square* 4 5 3 7 8 6) 2 #'heuristic) ;6
       (tf:error-if-failed1 (create-state 1 2 3 4 *blank-square* 5 7 8 6) 2 #'heuristic) ;4
       (tf:error-if-failed1 (create-state *blank-square* 2 4 6 8 1 3 5 7) 16 #'heuristic);34
       (format t "~&All ~S tests completed.~%" #'heuristic)
   )
)

(defun test-all-puzzle-search()
  (test-is-legal)
  (test-find-loc)
  (test-is-legal-move)
  (test-move)
  (test-generate-child-states)
  (test-man-distance)
  (test-heuristic)
)

