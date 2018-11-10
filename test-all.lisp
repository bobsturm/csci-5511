;; helper function.  raises error if actual is not equal to expected
(defun assert-actual (actual expected)
   (if (equal actual expected)
       (format t "test passed!~%")
        (progn 
           (format t "TEST FAILED! actual:~S" actual)
           (error "TEST FAILED!! Expected Value was ~S but actual was ~S" expected actual)
        )
    )
)


;; helper function.  raises an error if calling fn does not result in an error being raised.
(defun assert-error-thrown (arg1 arg2 fn)
   (format t "calling ~S with arg1:~S, arg2:~S. expecting error to be raised..." fn arg1 arg2)
   (handler-case 
      (progn
         (funcall fn arg1 arg2)
         (error "TEST FAILED! Failed to catch error with args ~S, ~S~%" arg1 arg2)
      )
      (error (err) (format t "Caught error:~S, as expected.~%" err))
   )
)

;;helper function.  calls the function being tested with 2 args.  Raises error if the actual value is not equal to the expected value.
(defun error-if-failed2(arg1 arg2 expected fn)
  (progn
     (format t "calling ~S with arg1:~S, arg2:~S,  expecting:~S..." fn arg1 arg2 expected)
     (let ((actual (funcall fn arg1 arg2)))
        (assert-actual actual expected)
     )
  )
)

;; helper function.  calls the function being tested with 1 arg.  Raises error if the actual value is not equal to the expected value.
(defun error-if-failed1(arg1 expected fn)
  (progn
     (format t "calling ~S with arg: ~S, expecting ~S...." fn arg1 expected)
     (let ((actual (funcall fn arg1)))
        (assert-actual actual expected)
     )
  )
)

;; these are the test cases for functionatom.
(defun test-functionatom()
  (progn (format t "Initiating tests for ~S...~%" #'functionatom) 
         (error-if-failed1 () () #'functionatom)
         (error-if-failed1 '(1) '(1) #'functionatom)
         (error-if-failed1 '((1)) '(1) #'functionatom)
         (error-if-failed1 '(1 2) '(1 2) #'functionatom)
         (error-if-failed1 '((1) 2) '(1 2) #'functionatom)
         (error-if-failed1 '(1 (2)) '(1 2) #'functionatom)
         (error-if-failed1 '(1 2 3) '(1 2 3) #'functionatom)
         (error-if-failed1 '(1 (2) 3) '(1 2 3) #'functionatom)
         (error-if-failed1 '((1) (2) (3)) '(1 2 3) #'functionatom)
         (error-if-failed1 '(1 ((((2))) 3)) '(1 2 3) #'functionatom)
         (error-if-failed1 '(1 (2 (3))) '(1 2 3) #'functionatom)
         (error-if-failed1 '(1 (2 (3)) 4) '(1 2 3 4) #'functionatom)
         (error-if-failed1 '(1 (2 (3)) 4 (5)) '(1 2 3 4 5) #'functionatom)
         (error-if-failed1 '(1 (2 3) (((4) 5 6))) '(1 2 3 4 5 6) #'functionatom)
         (error-if-failed1 '(1 2 3 4 5 (6)) '(1 2 3 4 5 6) #'functionatom)
         (format t "All ~S tests completed.~%" #'functionatom)
   )
)

;; these are the test cases for functionatom.
(defun test-replaceword()
  (progn (format t "Initiating tests for ~S...~%" #'replaceword) 
         (assert-error-thrown () () #'replaceword)
         (assert-error-thrown 'a 'a  #'replaceword)
         (error-if-failed2 'a () () #'replaceword)
         (error-if-failed2 'a '(a) 'YYYY #'replaceword)
         (format t "All ~S tests completed.~%" #'replaceword)
    )
)

;; this is the main test driver for all 3 parts to the assignment
(defun test-all()
  (progn (test-functionatom)
         (test-replaceword)
         "ALL TESTS PASSED!"
  )
)
