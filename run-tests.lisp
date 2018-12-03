;;;; Automates the loading and full regression suite tests for all CSCI 5511 assignments and extra credit

(in-package :cl-user)

;;; Orchestrates invocation of all tests for all assignments and extra credit.
(defun test-all()
  (test-functionatom)
  (test-replaceword)
  (test-all-a-star-search)
  (test-all-puzzle-search)
  "ALL TESTS PASSED!"
)

;;; Loads all files in dependency order and invokes the test suite for a full regression test.
(defun run-tests ()
  (format t "Loading tests and functions...")
  (cd "~/Documents/personal/university-of-minnesota/csci-5511-artificial-intelligence/csci-5511")
  (load "a-star-search.lisp")
  (load "test-utilities.lisp")
  (load "test-functionatom.lisp")
  (load "test-replaceword.lisp")
  (load "test-a-star-search.lisp")
  (load "test-puzzle-search.lisp")
  (load "functionatom.lisp")
  (load "replaceword.lisp")
  (load "puzzle-search.lisp")
  (format t "~&Loading completed.  Invoking all tests...~%")
  (test-all)
)
