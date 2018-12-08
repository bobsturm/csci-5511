;;;; Test regression suite for the a-star-search package.  Call test-all-a-star-search to invoke the test suite.
;;;; NOTE: This test suite is not as comprehensive as it should be!
;;;;   There is low coverage on a-star-search, moderate coverage of create-child-nodes, and very low coverage for tree-to-list.  All other functions are tested indirectly, which is not ideal.

(in-package :cl-user)

;;; Mock functions.  These provide the ability to test a-star-search in an isolated manner.  This is a toy set of mock functions.  Here is the game:
;;;   State is represented by a number.
;;;   Legal moves are to add one, subtract one, divide by 2, multiply by 2.  The floor is taken when dividing by 2.
;;;   The goal state is to get to 10.
;;;   The optimal path is to get to 10 with the fewest operations.
;;;   State must be between 0 and 999

;;; mock heuristic: returns the absolute value of (10 - futurestate)
(defun mock-heuristic (curstate futurestate)
   (abs (- futurestate 10))
)

;;; mock cost function
(defun mock-g-cost (curcost prevstate curstate)
   (if (null prevstate)
       0 ; the root node is the only node with prevstate of nil
       (+ curcost 1)
   )
)


;;; generates list of legal moves - within 0 to 999
;;; division is only allowed on even parent states
(defun mock-generate-child-states (parentstate)
   (remove-if 
      #'(lambda (state) (or (< state 0) (> state 999) (equal state parentstate))) (list
                                                                                      (+ parentstate 1)
                                                                                      (- parentstate 1)
                                                                                      (* parentstate 2)
                                                                                      (floor (/ parentstate 2)))
   )
)

;;; Prints the state for the mock game.
(defun mock-get-printable-state (state)
   (format nil "~S" state)
)

;;; Mock equals for 2 numbers
(defun mock-state-equal (state1 state2)
   (equal state1 state2)
)

;;; Test suite for create-child-nodes
(defun test-create-child-nodes()
   (format t "Initiating tests for ~S...~%" #'a-star:create-child-nodes)
   (let ((root  (a-star:create-node nil "ROOT" 99  0 nil)))
      (tf:error-if-failed4  root #'mock-generate-child-states #'mock-g-cost #'mock-heuristic (list 
                                                                             (a-star:create-node root "1" 100 1 90)
                                                                             (a-star:create-node root "2" 98 1 88)
                                                                             (a-star:create-node root "3" 198 1 188)
                                                                             (a-star:create-node root "4" 49 1 39)
                                                                             )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (a-star:create-node nil "ROOT" 9  0 nil)))
      (tf:error-if-failed4  root #'mock-generate-child-states #'mock-g-cost #'mock-heuristic (list 
                                                                             (a-star:create-node root "1" 10 1 0)
                                                                             (a-star:create-node root "2" 8 1 2)
                                                                             (a-star:create-node root "3" 18 1 8)
                                                                             (a-star:create-node root "4" 4 1 6)
                                                                             )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (a-star:create-node nil "ROOT" 0  0 nil)))
      (tf:error-if-failed4  root #'mock-generate-child-states #'mock-g-cost #'mock-heuristic (list 
                                                                             (a-star:create-node root "1" 1 1 9)
                                                                             )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (a-star:create-node nil "ROOT" 10000  0 nil)))
      (tf:error-if-failed4  root #'mock-generate-child-states #'mock-g-cost #'mock-heuristic nil #'a-star:create-child-nodes)
   )
   (format t "~&All ~S tests completed.~%" #'a-star:create-child-nodes)
)

;;; Utility function that creates a complex tree for testing purposes.
(defun create-test-tree ()
   (let ((expected-root (a-star:create-node nil "ROOT" 999 0 989)))
       (let ((c2 (a-star:create-node expected-root "2" 499 1 489)))
          (let ((c24 (a-star:create-node c2 "4" 249 2 239)))
             (let ((c244 (a-star:create-node c24 "4" 124 3 114)))
                (let ((c2444 (a-star:create-node c244 "4" 62 4 52)))
                   (let ((c24444 (a-star:create-node c2444 "4" 31 5 21)))
                      (let ((c244444 (a-star:create-node c24444 "4" 15 6 5)))
                         (let ((c2444444 (a-star:create-node c244444 "4" 7 7 3)))
                            (let ((c24444441 (a-star:create-node c2444444 "1" 8 8 2)))
                               (let ((c244444411 (a-star:create-node c24444441 "1" 9 9 1)))
                                  (let ((c2444444111 (a-star:create-node c244444411 "1" 10 10 0)))
                                     (a-star:set-children expected-root (list (a-star:create-node expected-root "1" 998 1 988)
                                                                              c2))
                                     (a-star:set-children c2            (list (a-star:create-node c2 "1" 500 2 490)
                                                                              (a-star:create-node c2 "2" 498 2 488)
                                                                              (a-star:create-node c2 "3" 998 2 988)
                                                                              c24))
                                     (a-star:set-children c24           (list (a-star:create-node c24 "1" 250 3 240)
                                                                              (a-star:create-node c24 "2" 248 3 238)
                                                                              (a-star:create-node c24 "3" 498 3 488)
                                                                              c244))
                                     (a-star:set-children c244          (list (a-star:create-node c244 "1" 125 4 115)
                                                                              (a-star:create-node c244 "2" 123 4 113)
                                                                              (a-star:create-node c244 "3" 248 4 238)
                                                                              c2444))
                                     (a-star:set-children c2444         (list (a-star:create-node c2444 "1" 63 5 53)
                                                                              (a-star:create-node c2444 "2" 61 5 51)
                                                                              (a-star:create-node c2444 "3" 124 5 114)
                                                                              c24444))
                                     (a-star:set-children c24444        (list (a-star:create-node c24444 "1" 32 6 22)
                                                                              (a-star:create-node c24444 "2" 30 6 20)
                                                                              (a-star:create-node c24444 "3" 62 6 52)
                                                                              c244444))
                                     (a-star:set-children c244444       (list (a-star:create-node c244444 "1" 16 7 6)
                                                                              (a-star:create-node c244444 "2" 14 7 4)
                                                                              (a-star:create-node c244444 "3" 30 7 20)
                                                                              c2444444))
                                     (a-star:set-children c2444444      (list c24444441
                                                                              (a-star:create-node c2444444 "2" 6 8 4)
                                                                              (a-star:create-node c2444444 "3" 14 8 4)
                                                                              (a-star:create-node c2444444 "4" 3 8 7)))
                                     (a-star:set-children c24444441     (list c244444411
                                                                              (a-star:create-node c24444441 "2" 7 9 3)
                                                                              (a-star:create-node c24444441 "3" 16 9 6)
                                                                              (a-star:create-node c24444441 "4" 4 9 6)))
                                     (a-star:set-children c244444411    (list c2444444111
                                                                              (a-star:create-node c244444411 "2" 8 10 2)
                                                                              (a-star:create-node c244444411 "3" 18 10 8)
                                                                              (a-star:create-node c244444411 "4" 4 10 6)))
         ))))))))))
       expected-root
   )
)

;;; tests for a-star-search
(defun test-a-star-search()
   (format t "Initiating tests for ~S...~%" #'a-star:a-star-search)
   (tf:error-if-failed8 100 10 10 #'mock-generate-child-states #'mock-heuristic #'mock-g-cost #'mock-state-equal #'mock-get-printable-state
                        (a-star:create-node nil "ROOT" 10 0 0) #'a-star:a-star-search)
   (let ((expected-root (a-star:create-node nil "ROOT" 9 0 1)))
      (a-star:set-children expected-root (list (a-star:create-node expected-root "1" 10 1 0)
                                               (a-star:create-node expected-root "2" 8 1 2)
                                               (a-star:create-node expected-root "3" 18 1 8)
                                               (a-star:create-node expected-root "4" 4 1 6)))
      (tf:error-if-failed8 100 9 10 #'mock-generate-child-states #'mock-heuristic #'mock-g-cost #'mock-state-equal #'mock-get-printable-state expected-root #'a-star:a-star-search)
   )
   (tf:error-if-failed8 100 999 10 #'mock-generate-child-states #'mock-heuristic #'mock-g-cost #'mock-state-equal #'mock-get-printable-state (create-test-tree) #'a-star:a-star-search)
   (format t "~&All ~S tests completed.~%" #'a-star:a-star-search)
)


;;; Tests for tree-to-list
(defun test-tree-to-list()
   (format t "Initiating tests for ~S...~%" #'a-star:tree-to-list)
   (tf:assert-actual (length (a-star:tree-to-list (create-test-tree))) 39)
   (format t "~&All ~S tests completed.~%" #'a-star:tree-to-list)
)


;;; Run all a-star tests.
(defun test-all-a-star-search()
  (test-create-child-nodes)
  (test-a-star-search)
  (test-tree-to-list)
)

