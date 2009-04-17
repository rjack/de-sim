(defpackage :asd.test.core
  (:use :cl :asdf))

(in-package :asd.test.core)


(defsystem de-sim.test.core
    :name "test.core"
    :author "Giacomo Ritucci"
    :version "0.1"
    :license "2 clauses BSD style, see COPYING file for details"
    :components ((:file "lisp-unit")
		 (:file "core")
		 (:file "test.core"
			:depends-on ("lisp-unit" "core"))))
