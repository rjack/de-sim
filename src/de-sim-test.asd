(defpackage :de-sim-test-asd
  (:use :cl :asdf))

(in-package :de-sim-test-asd)


(defsystem de-sim-test
    :name "de-sim-test"
    :author "Giacomo Ritucci"
    :version "0.1"
    :license "2 clauses BSD style, see COPYING file for details"
    :depends-on ("de-sim")
    :components ((:file "lisp-unit")
		 (:file "de-sim-test"
			:depends-on ("lisp-unit"))))
