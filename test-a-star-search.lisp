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

;;; helper function to make creating a node with a cost easy
(defun create-node-helper (parent name state h_n g_n)
   (let ((node (a-star:create-node parent name state h_n)))  
      (a-star:set-g_n node g_n)
      node
   )
)

(defun test-create-child-nodes()
   (format t "Initiating tests for ~S...~%" #'a-star:create-child-nodes)
   (let ((root  (create-node-helper nil "ROOT" 99  nil nil)))
      (tf:error-if-failed3  root #'mock-generate-child-states #'mock-heuristic (list 
                                                                             (create-node-helper root "1" 100 90 nil)
                                                                             (create-node-helper root "2" 98 88 nil)
                                                                             (create-node-helper root "3" 198 188 nil)
                                                                             (create-node-helper root "4" 49 39 nil)
                                                                          )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (create-node-helper nil "ROOT" 9  nil nil)))
      (tf:error-if-failed3  root #'mock-generate-child-states #'mock-heuristic (list 
                                                                             (create-node-helper root "1" 10 0 nil)
                                                                             (create-node-helper root "2" 8 2 nil)
                                                                             (create-node-helper root "3" 18 8 nil)
                                                                             (create-node-helper root "4" 4 6 nil)
                                                                             )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (create-node-helper nil "ROOT" 0  nil nil)))
      (tf:error-if-failed3  root #'mock-generate-child-states #'mock-heuristic (list 
                                                                             (create-node-helper root "1" 1 9 nil)
                                                                             )
                                                                          #'a-star:create-child-nodes)
   )
   (let ((root  (create-node-helper nil "ROOT" 10000  nil nil)))
      (tf:error-if-failed3  root #'mock-generate-child-states #'mock-heuristic nil #'a-star:create-child-nodes)
   )
   (format t "~&All ~S tests completed.~%" #'a-star:create-child-nodes)
)

(defun create-test-tree ()
   (let ((expected-root (create-node-helper nil "ROOT" 999 989 0)))
       (let ((c2 (create-node-helper expected-root "2" 499 489 1)))
          (let ((c24 (create-node-helper c2 "4" 249 239 2)))
             (let ((c244 (create-node-helper c24 "4" 124 114 3)))
                (let ((c2444 (create-node-helper c244 "4" 62 52 4)))
                   (let ((c24444 (create-node-helper c2444 "4" 31 21 5)))
                      (let ((c244444 (create-node-helper c24444 "4" 15 5 6)))
                         (let ((c2444444 (create-node-helper c244444 "4" 7 3 7)))
                            (let ((c24444441 (create-node-helper c2444444 "1" 8 2 8)))
                               (let ((c244444411 (create-node-helper c24444441 "1" 9 1 9)))
                                  (let ((c2444444111 (create-node-helper c244444411 "1" 10 0 10)))
                                     (a-star:set-children expected-root (list (create-node-helper expected-root "1" 998 988 nil)
                                                                              c2))
                                     (a-star:set-children c2            (list (create-node-helper c2 "1" 500 490 nil)
                                                                              (create-node-helper c2 "2" 498 488 nil)
                                                                              (create-node-helper c2 "3" 998 988 nil)
                                                                              c24))
                                     (a-star:set-children c24           (list (create-node-helper c24 "1" 250 240 nil)
                                                                              (create-node-helper c24 "2" 248 238 nil)
                                                                              (create-node-helper c24 "3" 498 488 nil)
                                                                              c244))
                                     (a-star:set-children c244          (list (create-node-helper c244 "1" 125 115 nil)
                                                                              (create-node-helper c244 "2" 123 113 nil)
                                                                              (create-node-helper c244 "3" 248 238 nil)
                                                                              c2444))
                                     (a-star:set-children c2444         (list (create-node-helper c2444 "1" 63 53 nil)
                                                                              (create-node-helper c2444 "2" 61 51 nil)
                                                                              (create-node-helper c2444 "3" 124 114 nil)
                                                                              c24444))
                                     (a-star:set-children c24444        (list (create-node-helper c24444 "1" 32 22 nil)
                                                                              (create-node-helper c24444 "2" 30 20 nil)
                                                                              (create-node-helper c24444 "3" 62 52 nil)
                                                                              c244444))
                                     (a-star:set-children c244444       (list (create-node-helper c244444 "1" 16 6 nil)
                                                                              (create-node-helper c244444 "2" 14 4 nil)
                                                                              (create-node-helper c244444 "3" 30 20 nil)
                                                                              c2444444))
                                     (a-star:set-children c2444444      (list c24444441
                                                                              (create-node-helper c2444444 "2" 6 4 nil)
                                                                              (create-node-helper c2444444 "3" 14 4 nil)
                                                                              (create-node-helper c2444444 "4" 3 7 nil)))
                                     (a-star:set-children c24444441     (list c244444411
                                                                              (create-node-helper c24444441 "2" 7 3 nil)
                                                                              (create-node-helper c24444441 "3" 16 6 nil)
                                                                              (create-node-helper c24444441 "4" 4 6 nil)))
                                     (a-star:set-children c244444411    (list c2444444111
                                                                              (create-node-helper c244444411 "2" 8 2 nil)
                                                                              (create-node-helper c244444411 "3" 18 8 nil)
                                                                              (create-node-helper c244444411 "4" 4 6 nil)))
         ))))))))))
       expected-root
   )
)

;;; tests for a-star-search
(defun test-a-star-search()
   (format t "Initiating tests for ~S...~%" #'a-star:a-star-search)
   (tf:error-if-failed7 10 10 #'mock-generate-child-states #'mock-heuristic #'mock-g-cost #'mock-state-equal #'mock-get-printable-state
                        (create-node-helper nil "ROOT" 10 0 0) #'a-star:a-star-search)
   (let ((expected-root (create-node-helper nil "ROOT" 9 1 0)))
      (a-star:set-children expected-root (list (create-node-helper expected-root "1" 10 0 1)
                                               (create-node-helper expected-root "2" 8 2 nil)
                                               (create-node-helper expected-root "3" 18 8 nil)
                                               (create-node-helper expected-root "4" 4 6 nil)))
      (tf:error-if-failed7 9 10 #'mock-generate-child-states #'mock-heuristic #'mock-g-cost #'mock-state-equal #'mock-get-printable-state expected-root #'a-star:a-star-search)
   )
   (tf:error-if-failed7 999 10 #'mock-generate-child-states #'mock-heuristic #'mock-g-cost #'mock-state-equal #'mock-get-printable-state (create-test-tree) #'a-star:a-star-search)
   (format t "~&All ~S tests completed.~%" #'a-star:a-star-search)
)

(defun test-is-cycle()
   (format t "Initiating tests for ~S...~%" #'a-star:is-cycle)
   (tf:error-if-failed3 nil nil #'mock-state-equal nil #'a-star:is-cycle)
   (tf:error-if-failed3 (create-node-helper nil "root" -1 0 0) nil #'mock-state-equal nil #'a-star:is-cycle)
   (tf:error-if-failed3 nil (create-node-helper nil "root" -1 0 0) #'mock-state-equal nil #'a-star:is-cycle)
   (let ((root (create-node-helper nil "root" 0 10 0)))
      (tf:error-if-failed3 root (create-node-helper root "1" 1 9 1) #'mock-state-equal nil #'a-star:is-cycle)
   )
   (let ((root (create-node-helper nil "root" 0 10 0)))
      (tf:error-if-failed3 root (create-node-helper root "1" 1 9 1) #'mock-state-equal nil #'a-star:is-cycle)
   )
   (let ((root (create-node-helper nil "root" 0 10 0)))
     (let ((child (create-node-helper root "1" 1 10 0)))
        (a-star:set-children root (list child))
        (tf:error-if-failed3 root  (create-node-helper child "2" 2 10 7) #'mock-state-equal nil #'a-star:is-cycle)
     )
   )
   (let ((root (create-node-helper nil "root" 0 10 0)))
     (let ((child (create-node-helper root "1" 1 10 0)))
        (a-star:set-children root (list child))
        (tf:error-if-failed3 root  (create-node-helper root "2" 2 10 7) #'mock-state-equal nil #'a-star:is-cycle)
     )
   )
   (let ((root (create-node-helper nil "root" 0 10 0)))
     (let ((child (create-node-helper root "1" 1 10 1)))
        (a-star:set-children root (list child))
        (tf:error-if-failed3 root (create-node-helper root "2" 1 10 7) #'mock-state-equal t #'a-star:is-cycle)
     )
   )
   (let ((root (create-node-helper nil "root" 0 10 0)))
     (let ((child (create-node-helper root "1" 1 10 1)))
        (a-star:set-children root (list child))
        (tf:error-if-failed3 root (create-node-helper child "2" 0 10 7) #'mock-state-equal t #'a-star:is-cycle)
     )
   )
   (tf:error-if-failed3 (create-test-tree) (create-node-helper nil "ABC" 9 10 70) #'mock-state-equal t #'a-star:is-cycle)
   (tf:error-if-failed3 (create-test-tree) (create-node-helper nil "ABC1" 6 0 0) #'mock-state-equal nil #'a-star:is-cycle)
   (tf:error-if-failed3 (create-test-tree) (create-node-helper nil "ABC2" 6 5 5) #'mock-state-equal nil #'a-star:is-cycle)
   (tf:error-if-failed3 (create-test-tree) (create-node-helper nil "ABC3" 999 10 70) #'mock-state-equal t #'a-star:is-cycle)
   (tf:error-if-failed3 (create-test-tree) (create-node-helper nil "ABC4" 10 0 0) #'mock-state-equal t #'a-star:is-cycle)
   (tf:error-if-failed3 (create-test-tree) (create-node-helper nil "ABC5" 31 0 0) #'mock-state-equal t #'a-star:is-cycle)
   (tf:error-if-failed3 (create-test-tree) (create-node-helper nil "ABC6" 31 0 nil) #'mock-state-equal nil #'a-star:is-cycle)
   (let ((multiple-children-expanded-tree (create-test-tree)))
      (let ((c242 (second (a-star:get-children (fourth (a-star:get-children (second (a-star:get-children multiple-children-expanded-tree))))))))
        ; make this node look expanded so it is considered for a cycle
        (a-star:set-g_n c242 99)
        (tf:error-if-failed3 multiple-children-expanded-tree (create-node-helper nil "ABC7" 248 0 98) #'mock-state-equal t #'a-star:is-cycle)
      )
   )
   (format t "~&All ~S tests completed.~%" #'a-star:is-cycle)
)

(defun test-tree-to-list()
   (format t "Initiating tests for ~S...~%" #'a-star:tree-to-list)
   (tf:assert-actual (length (a-star:tree-to-list (create-test-tree))) 39)
   (format t "~&All ~S tests completed.~%" #'a-star:tree-to-list)
)


(defun test-all-a-star-search()
  (test-create-child-nodes)
  (test-a-star-search)
  (test-is-cycle)
  (test-tree-to-list)
)

