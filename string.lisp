(defpackage :cl-string
  (:documentation "Contains a collection of useful string manipulation 
                   functions")
  (:use :common-lisp)
  (:export :string-make
           :string-append
           :string-char
           :string-length
           :string-to-list
           :string-from-char-list
           :string-containsp
           :string-split-by-position
           :string-split-by-chars
           :string-clear!
           :string-case-sensitive=
           :string-case-insensitive=
           :string==
           :string/==
           :string-resizablep))

(in-package :cl-string)

(defun string-iter (string func)
  (loop :for chr :across string
     :do (funcall func chr)))

(defun string-compare-two (string-one string-two &key (case-sensitive nil))
  (if case-sensitive
      (string= string-one string-two)
      (string-equal string-one string-two)))

(defun string-compare-one-string-against-list (string-to-be-compared-to-others
                                               compare-function
                                               others)
  (loop for one-of-the-others in others
     for equal = (funcall compare-function
                          string-to-be-compared-to-others
                          one-of-the-others)
     while equal
     finally (return equal)))

(defun string-case-sensitive= (string-to-be-compared-to-others
                              &rest others)
  (string-compare-one-string-against-list
     string-to-be-compared-to-others
     (lambda (a b) (string-compare-two a b :case-sensitive t))
     others))

(defun string== (string-to-be-compared-to-others &rest others)
  (apply #'string-case-sensitive=  string-to-be-compared-to-others others))

(defun string/== (string-to-be-compared-to-others &rest others)
  (not (apply #'string== string-to-be-compared-to-others others)))

(defun string-case-insensitive= (string-to-be-compared-to-others
                                       &rest others)
  (string-compare-one-string-against-list
     string-to-be-compared-to-others
     (lambda (a b) (string-compare-two a b :case-sensitive nil))
     others))


(defun string-make (&key (preallocate-chars 0 supplied-preallocate-chars-p)
                      (to-string nil supplied-to-string-p))
  (let* ((to-string (when supplied-to-string-p (format nil "~a" to-string)))
         (length (cond (supplied-preallocate-chars-p preallocate-chars)
                       (supplied-to-string-p (length to-string))
                       (t 0)))
	 (result (make-array length
			     :fill-pointer 0
			     :adjustable t
			     :element-type 'character)))
    (when supplied-to-string-p 
      (string-iter to-string (lambda (char) (vector-push-extend char result))))
    result))

(defun string-resizablep (str)
  (array-has-fill-pointer-p str))

(defun string-check-fill-pointer (str)
  "Signals an error if the string is has not a fill pointer"
  (unless (array-has-fill-pointer-p str)
    (error "The string need a fill pointer. This is done in the array creation.
            But to make it easy, just use the functino string-make, but be sure
            to NOT use make-string."))
  nil)

(defun string-append (string &rest values)
  "Append to an string anything you want. The first argument must be
   an STRING and the &REST arguments can be of any type"
  (let* ((string (cond
                   ((and (stringp string) (string-resizablep string)) string)
                   (string (string-make :to-string string))
                   (t (string-make))))
         (to-append (loop :for val :in values
                       :collecting (cond ((stringp val) val)
                                         (t (string-make :to-string val)))))
	 (reslength (loop :for val :in to-append :summing (length val)))
	 (result (string-make :preallocate-chars reslength))
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


(defun string-split-by-function (string-to-be-splited split-test-function)
  "SPLIT-TEST-FUNCTION takes two arguments, the first is the whole string, and
   the second is an integral number wich indicates the split position."
  (let ((result))
    (flet ((call-test-function-at-index (index)
             (funcall split-test-function string-to-be-splited index))
           (add-to-result (x) (when x (setf result (cons x result))))
           (reverse-result () (setf result (nreverse result))))
      (loop :for char :across string-to-be-splited
         :for char-index = 0 then (1+ char-index)
         :for substr = (string-append substr char)
         :with result = nil
         :when (call-test-function-at-index char-index)
         :do (progn
               (add-to-result substr)
               (setf substr nil))
         :finally (return (add-to-result substr)))
      (reverse-result))))

(defun string-split-by-position (string-to-be-splited &rest positions)
  (funcall #'string-split-by-function string-to-be-splited
           (lambda (_ index) (declare (ignore _)) (member index positions))))

(defun string-split-by-chars (string-to-be-splited &rest characters)
  (funcall #'string-split-by-function string-to-be-splited
           (lambda (string index) (let ((char (string-char string index)))
                               (member char characters)))))

(defun string-clear! (str)
  "Returns the fill pointer to 0, this means that the string will be empty
   again, without reallocation, and without erasing. And therefore this 
   function requires an string with fill pointer. Returns the modified string.
   Note that any character beyond the fill pointer is garbage collected."
  (string-check-fill-pointer str)
  (setf (fill-pointer str) 0)
  str)
