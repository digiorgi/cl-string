
(asdf:defsystem "cl-string"
  :description "cl-string is a simple collection of function dedicated to
                simplify the existing string functions."
  :version "0.0.1"
  :author "Di Giorgi Hernan Ezequiel <contact@playnu.com.ar>"
  :license "Simplified BSD License"
  :in-order-to ((test-op (load-op :cl-string/test)))
  :perform (test-op (o s) (asdf:test-system :cl-string/test))
  :components ((:file "string")))

(asdf:defsystem cl-string/test
    :depends-on (cl-string clunit)
    :perform (test-op (o s)
                      (uiop:symbol-call :cl-string/test '#:run-tests))
    :components ((:file "test/test-string")))
