(defpackage :cl-string/test
  (:documentation "Unit tests of cl-string")
  (:use :common-lisp :cl-string :clunit))

(in-package :cl-string/test)

(defsuite cl-string ())

(defun run-tests ()
  (print (clunit:run-suite 'cl-string))
  nil)

(deftest test-string-length (cl-string)
  (assert-equalp 5 (string-length "CAS_A")))

(deftest test-string-equality (cl-string)
  (assert-true  (string-case-insensitive= "TEST" "Test" "TEst" "test" "TESt"))
  (assert-true  (string-case-sensitive= "TEST" "TEST" "TEST"))
  (assert-false (string-case-insensitive= "TEST " "TEST" "test "))
  (assert-false (string-case-insensitive= "TEST" "TOAST" "SEE" "ZOO-$"))
  (assert-false (string-case-sensitive= "tEST" "Test" "test"))
  (assert-true  (string== "hello world" "hello world"))
  (assert-false (string== "hello_" "heLLO_"))
  (assert-true  (string/== "MOON" "MOON" "MOON" "moon")))

(deftest test-string-make (cl-string)
  (assert-true (string== "Hello" (string-make :to-string "Hello")))
  (assert-true (string== "" (string-make)))
  (assert-true (string== "" (string-make :preallocate-chars 10)))
  (assert-true (string== "1276" (string-make :to-string 1276) )))

(deftest test-string-append (cl-string)
  (assert-true (string== "A" (string-append nil "A")))
  (assert-true (string== "AAAA" (string-append "AA" "A" "A")))
  (assert-true (string== "AA-AA"
                         (string-append (string-make :to-string "AA")
                                        "-" "A" "A")))
  (assert-true (string== "1A124 124" (string-append 1 "A" 124 " " 124))))

(deftest test-string-split (cl-string)
  (assert-equalp '("HEL" "L" "O") (string-split-by-chars "HELLO" #\L))
  (assert-equalp '("HEL" "L" "O") (string-split-by-position "HELLO" 2 3 4)))
