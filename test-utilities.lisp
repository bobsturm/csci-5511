;;;; TESTING FUNCTIONS PACKAGE
;;;;
;;;; These testing functions form the basis for creating tests for functions.
;;;;

(in-package :cl-user)

(defpackage tf
   (:export :assert-actual :assert-error-thrown :error-if-failed1 :error-if-failed2 :error-if-failed3 :error-if-failed4 :error-if-failed8)
)
(in-package :tf)

;;; Raises error if actual result from a function call is not equal to the expected result
(defun assert-actual (actual expected)
   (if (not (equalp actual expected))
      (progn 
         (format t "~&TEST FAILED!~%******ACTUAL:~S~%****EXPECTED:~S" actual expected)
         (error "TEST FAILED!! Expected Value was ~S but actual was ~S" expected actual)
      )
    )
)


;;; Use this function when you expect an error to be raised.  Calls the given function and handles the expected error.  If
;;; an error is not thrown, this function will raise an error indicating that the test failed.
(defun assert-error-thrown (arg1 arg2 fn)
   (format t "~&calling ~S with arg1:~S, arg2:~S. expecting error to be raised..." fn arg1 arg2)
   (handler-case 
      (progn
         (funcall fn arg1 arg2)
         (error "TEST FAILED! Failed to catch error with args ~S, ~S~%" arg1 arg2)
      )
      (error (err) (format t "Caught error:~S, as expected.~%" err))
   )
)

;;; Use this testing helper to invoke a function with 1 arg and validate that the given expected result is
;;; equal -- using equalp -- to the actual result from invoking the function with the argument given.
(defun error-if-failed1(arg1 expected fn)
  (progn
     (format t "~&calling ~S with arg: ~S, expecting ~S...." fn arg1 expected)
     (let ((actual (funcall fn arg1)))
        (assert-actual actual expected)
     )
  )
)


;;; Use this testing helper to invoke a function with 2 arguments and validate that the given expected result is
;;; equal -- using equalp -- to the actual result from invoking the function with the arguments given.
(defun error-if-failed2(arg1 arg2 expected fn)
  (progn
     (format t "~&calling ~S with arg1:~S, arg2:~S, expecting:~S..." fn arg1 arg2 expected)
     (let ((actual (funcall fn arg1 arg2)))
        (assert-actual actual expected)
     )
  )
)

;;; Use this testing helper to invoke a function with 3 arguments and validate that the given expected result is
;;; equal -- using equalp -- to the actual result from invoking the function with the arguments given.
(defun error-if-failed3 (arg1 arg2 arg3 expected fn)
  (progn
     (format t "~&calling ~S with arg1:~S, arg2:~S, arg3:~S, expecting:~S..." fn arg1 arg2 arg3 expected)
     (let ((actual (funcall fn arg1 arg2 arg3)))
        (assert-actual actual expected)
     )
  )
)

;;; Use this testing helper to invoke a function with 4 arguments and validate that the given expected result is
;;; equal -- using equalp -- to the actual result from invoking the function with the arguments given.
(defun error-if-failed4 (arg1 arg2 arg3 arg4 expected fn)
  (progn
     (format t "~&calling ~S with arg1:~S, arg2:~S, arg3:~S, arg4~S, expecting:~S..." fn arg1 arg2 arg3 arg4 expected)
     (let ((actual (funcall fn arg1 arg2 arg3 arg4)))
        (assert-actual actual expected)
     )
  )
)

;;; Use this testing helper to invoke a function with 8 arguments and validate that the given expected result is
;;; equal -- using equalp -- to the actual result from invoking the function with the arguments given.
(defun error-if-failed8 (arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 expected fn)
   (format t "~&calling ~S with arg1:~S, arg2:~S, arg3:~S, arg4:~S, arg5:~S, arg6:~S, arg7:~S, arg8:~S; expecting:~S..." fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 expected)
   (assert-actual (funcall fn arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8) expected)
)
