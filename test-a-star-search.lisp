;;;; Test regression suite for the a-star-search package.  Call test-all-a-star-search to invoke the test suite.

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


(defun test-create-child-nodes()
   (format t "Initiating tests for ~S...~%" #'a-star:create-child-nodes)
   (let ((root  (a-star:create-node nil "ROOT" 99  nil)))
      (tf:error-if-failed3  root #'mock-generate-child-states #'mock-heuristic (list 
                                                                             (a-star:create-node root "1" 100 90)
                                                                             (a-star:create-node root "2" 98 88)
                                                                             (a-star:create-node root "3" 198 188)
                                                                             (a-star:create-node root "4" 49 39)
                                                                          )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (a-star:create-node nil "ROOT" 9  nil)))
      (tf:error-if-failed3  root #'mock-generate-child-states #'mock-heuristic (list 
                                                                             (a-star:create-node root "1" 10 0)
                                                                             (a-star:create-node root "2" 8 2)
                                                                             (a-star:create-node root "3" 18 8)
                                                                             (a-star:create-node root "4" 4 6)
                                                                             )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (a-star:create-node nil "ROOT" 0  nil)))
      (tf:error-if-failed3  root #'mock-generate-child-states #'mock-heuristic (list 
                                                                             (a-star:create-node root "1" 1 9)
                                                                             )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (a-star:create-node nil "ROOT" 10000  nil)))
      (tf:error-if-failed3  root #'mock-generate-child-states #'mock-heuristic nil #'a-star:create-child-nodes)
   )
   (format t "All ~S tests completed.~%" #'a-star:create-child-nodes)
)

;;; tests for a-star-search
(defun test-a-star-search()
   (format t "Initiating tests for ~S...~%" #'a-star:a-star-search)
   (tf:error-if-failed6 10 10 #'mock-generate-child-states #'mock-heuristic #'mock-state-equal #'mock-get-printable-state
                        (a-star:create-node nil "ROOT" 10 0) #'a-star:a-star-search)
   (let ((expected-root (a-star:create-node nil "ROOT" 9 1)))
      (a-star:set-children expected-root (list (a-star:create-node expected-root "1" 10 0)
                                               (a-star:create-node expected-root "2" 8 2)
                                               (a-star:create-node expected-root "3" 18 8)
                                               (a-star:create-node expected-root "4" 4 6)))
      (tf:error-if-failed6 9 10 #'mock-generate-child-states #'mock-heuristic #'mock-state-equal #'mock-get-printable-state expected-root #'a-star:a-star-search)
   )
   (let ((expected-root (a-star:create-node nil "ROOT" 999 989)))
       (let ((c2 (a-star:create-node expected-root "2" 499 489)))
          (let ((c24 (a-star:create-node c2 "4" 249 239)))
             (let ((c244 (a-star:create-node c24 "4" 124 114)))
                (let ((c2444 (a-star:create-node c244 "4" 62 52)))
                   (let ((c24444 (a-star:create-node c2444 "4" 31 21)))
                      (let ((c244444 (a-star:create-node c24444 "4" 15 5)))
                         (let ((c2444444 (a-star:create-node c244444 "4" 7 3)))
                            (let ((c24444441 (a-star:create-node c2444444 "1" 8 2)))
                               (let ((c244444411 (a-star:create-node c24444441 "1" 9 1)))
                                  (let ((c2444444111 (a-star:create-node c244444411 "1" 10 0)))
                                     (a-star:set-children expected-root (list (a-star:create-node expected-root "1" 998 988)
                                                                              c2))
                                     (a-star:set-children c2            (list (a-star:create-node c2 "1" 500 490)
                                                                              (a-star:create-node c2 "2" 498 488)
                                                                              (a-star:create-node c2 "3" 998 988)
                                                                              c24))
                                     (a-star:set-children c24           (list (a-star:create-node c24 "1" 250 240)
                                                                              (a-star:create-node c24 "2" 248 238)
                                                                              (a-star:create-node c24 "3" 498 488)
                                                                              c244))
                                     (a-star:set-children c244          (list (a-star:create-node c244 "1" 125 115)
                                                                              (a-star:create-node c244 "2" 123 113)
                                                                              (a-star:create-node c244 "3" 248 238)
                                                                              c2444))
                                     (a-star:set-children c2444         (list (a-star:create-node c2444 "1" 63 53)
                                                                              (a-star:create-node c2444 "2" 61 51)
                                                                              (a-star:create-node c2444 "3" 124 114)
                                                                              c24444))
                                     (a-star:set-children c24444        (list (a-star:create-node c24444 "1" 32 22)
                                                                              (a-star:create-node c24444 "2" 30 20)
                                                                              (a-star:create-node c24444 "3" 62 52)
                                                                              c244444))
                                     (a-star:set-children c244444       (list (a-star:create-node c244444 "1" 16 6)
                                                                              (a-star:create-node c244444 "2" 14 4)
                                                                              (a-star:create-node c244444 "3" 30 20)
                                                                              c2444444))
                                     (a-star:set-children c2444444      (list c24444441
                                                                              (a-star:create-node c2444444 "2" 6 4)
                                                                              (a-star:create-node c2444444 "3" 14 4)
                                                                              (a-star:create-node c2444444 "4" 3 7)))
                                     (a-star:set-children c24444441     (list c244444411
                                                                              (a-star:create-node c24444441 "2" 7 3)
                                                                              (a-star:create-node c24444441 "3" 16 6)
                                                                              (a-star:create-node c24444441 "4" 4 6)))
                                     (a-star:set-children c244444411    (list c2444444111
                                                                              (a-star:create-node c244444411 "2" 8 2)
                                                                              (a-star:create-node c244444411 "3" 18 8)
                                                                              (a-star:create-node c244444411 "4" 4 6)))
         )))))))))) ;close out all but first let
        (tf:error-if-failed6 999 10 #'mock-generate-child-states #'mock-heuristic #'mock-state-equal #'mock-get-printable-state expected-root #'a-star:a-star-search)
   )
   (format t "All ~S tests completed.~%" #'a-star:a-star-search)
)

(defun test-all-a-star-search()
  (test-create-child-nodes)
  (test-a-star-search)
)

