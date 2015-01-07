(defpackage :cl-string
  (:documentation "Contains a collection of useful string manipulation 
                   functions")
  (:use :common-lisp))

(in-package :cl-string)

(defun string-iter (string func)
  (loop :for chr :across string
	:do (funcall func chr)))

(defun string-make (&optional arg)
  "Creates a resizable string, This function accepts one optional argument, 
   it can be a STRING, to create a resizable string of the same size or a
   NUMBER, to create a resizable string of the specified size"
  (let* ((length (or (and (numberp arg) arg)
		     (and (stringp arg) (length arg))
		     (and (characterp arg) 1)
		     0))
	 (result (make-array length
			     :fill-pointer 0
			     :adjustable t
			     :element-type 'character)))
    (cond
      ((stringp arg) (string-iter arg (lambda (char)
				    (vector-push-extend char result))))
      ((characterp arg) (vector-push-extend arg result)))
    result))

(defun string-append (string &rest values)
  "Append to an string anything you want. The first argument must be
   an STRING and the &REST arguments can be of any type"
  (let* ((to-append (loop :for val :in values
			  :collecting
			  (or (and (stringp val) val)
			      (and (characterp val) (string-make val) )
			      (and val (write-to-string val))
			      0)))
	 (reslength (loop :for val :in to-append
			  :collecting (length val)))
	 (result (string-make reslength))
	 (push-char (lambda (chr) (vector-push-extend chr result)))
	 (append (lambda (to-append)
		   (string-iter to-append (lambda (char)
					    (funcall push-char char))))))
    (if string (funcall append string))
    (loop :for val :in to-append :do
      (funcall append val))
    result))

(defmacro string-char (text index)
  "Gets the character of TEXT at zero based INDEX"
  `(char ,text ,index))

(defmacro string-length (text)
  "Gets the lenght of the string"
  `(length ,text))

(defmacro string-to-list (text)
  "Transform the string in to a list"
  `(coerce ,text 'list))

(defmacro string-from-char-list (list)
  "Transform a list of character to a string."
  `(coerce ,list 'string))

(defun string-containsp (text to-test)
  ""
  (let* ((chars (typecase to-test
		  (character (list to-test))
		  (string (string-to-list to-test))
		  (list to-test)))
	 (has-char (lambda (c) (when (member c chars) t))))
    (do* ((len (string-length text))
	  (contains nil)
	  (i 0 (1+ i)))
	 ((or (>= i len) contains) contains)
      (setf contains
	    (funcall has-char (string-char text i))))))

(defun string-split (str &key (pos 0 supplied-pos)
			   (char nil supplied-char)
			   (function nil supplied-function)
			   (remove-char t))
  "Split an string in a given position with
   :POS an integer o list of integer that represent a zero index based position
   where the string will be split. :CHAR the split is done in each occurrence 
   of the character passed, but :CHAR also can be an STRING. 
   :FUNCTION the split id done with a function that accepts one string 
   returns true."
  (let* ((split-test
	   (lambda (index string)
	     (cond
	       (supplied-pos
		(or (and (numberp pos) (equalp index pos))
		    (and (listp pos) (member index pos))))
	       (supplied-char
		(typecase char
		  (character (char= (char string index) char))
		  (string (string-containsp char
					    (string-char string index)))))
	       (supplied-function (funcall function index string)))))
	 (result (list))
	 (subresult)
	 (push-subresult (lambda ()
			   (when subresult
			     (setf result (cons subresult result))
			     (setf subresult nil)
			     result))))
    (loop :for char :across str
	  :for x = 0 :then (1+ x)
	  :do
	     (let ((test-result (funcall split-test x str)))
	       (if (or (not test-result)
		       (and test-result (not remove-char)))
		   (setf subresult (string-append subresult char)))
	       (if test-result
		 (funcall push-subresult)))
	  :finally (if subresult (funcall push-subresult)))
    (nreverse result)))

(defun string-last-character-split (string chr)
  "Split the STRING using a character CHR, and return the last result"
  (car (last (string-split string :char chr))))

(defun string-start-withp (str1 str2 &optional (start 0))
  "Verifies if STR1 has the STR2 in the position START."
  (let ((len1 (length str1))
	(len2 (length str2))
	(comparation t))
    (if (> len2 len1) nil
	(do ((i1 start (1+ i1)) (i2 0 (1+ i2)))
	    ((or (not comparation) (>= i2 len2)) comparation)
	  (format t "compare: ~a vs ~a~%" (char str1 i1)
			(char str2 i2))
	  (setf comparation
		(equalp (char str1 i1)
			(char str2 i2)))))))

(defun string-check-fill-pointer (str)
  "Signals an error if the string is has not a fill pointer"
  (unless (array-has-fill-pointer-p str)
    (error "The string need a fill pointer. This is done in the array creation.
            But to make it easy, just use the functino string-make, but be sure
            to NOT use make-string."))
  nil)

(defun string-nclear (str)
  "Returns the fill pointer to 0, this means that the string will be empty
   again, without reallocation, and without erasing. And therefore this 
   function requires an string with fill pointer. Returns the modified string.
   Note that any character beyond the fill pointer is garbage collected."
  (string-check-fill-pointer str)
  (setf (fill-pointer str) 0)
  str)
