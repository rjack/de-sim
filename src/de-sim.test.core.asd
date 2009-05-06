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
		 (:file "util"
			:depends-on ("core"))
		 (:file "test.core"
			:depends-on ("util" "lisp-unit" "core"))
		 (:file "test.util"
			:depends-on ("util" "lisp-unit" "core"))))
