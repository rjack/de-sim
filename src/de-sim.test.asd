(defpackage :de-sim.test.asd
  (:use :cl :asdf))

(in-package :de-sim.test.asd)


(defsystem de-sim.test
    :name "test.core"
    :author "Giacomo Ritucci"
    :version "0.1"
    :license "2 clauses BSD style, see COPYING file for details"
    :components ((:file "lisp-unit")
		 (:file "core")
		 (:file "util"
			:depends-on ("core"))
		 (:file "test.core"
			:depends-on ("util" "lisp-unit" "core"))
		 (:file "test.util"
			:depends-on ("util" "lisp-unit" "core"))))
