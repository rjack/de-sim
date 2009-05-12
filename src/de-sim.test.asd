(defpackage :de-sim.test.asd
  (:use :cl :asdf))

(in-package :de-sim.test.asd)


(defsystem de-sim.test
    :name "test.core"
    :author "Giacomo Ritucci"
    :version "0.1"
    :license "2 clauses BSD style, see COPYING file for details"
    :components ((:file "lisp-unit")
		 (:file "conditions")
		 (:file "core"
			:depends-on ("conditions"))
		 (:file "buffer"
			:depends-on ("core" "conditions" "util"))
		 (:file "util"
			:depends-on ("core" "conditions"))
		 (:file "test.core"
			:depends-on ("lisp-unit" "core" "util"))
		 (:file "test.util"
			:depends-on ("lisp-unit" "core" "util"))
		 (:file "test.buffer"
			:depends-on ("lisp-unit" "core" "conditions" "util" "buffer"))))
