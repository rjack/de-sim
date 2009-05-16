(defpackage :de-sim.asd
  (:use :cl :asdf))

(in-package :de-sim.asd)


(defsystem de-sim
    :name "de-sim"
    :author "Giacomo Ritucci"
    :version "0.1"
    :license "2 clauses BSD style, see COPYING file for details"
    :components ((:file "de-sim.package")
		 (:file "conditions"
			:depends-on ("de-sim.package"))
		 (:file "core"
			:depends-on ("de-sim.package" "conditions"))
		 (:file "util"
			:depends-on ("de-sim.package" "conditions"))
		 (:file "buffer"
			:depends-on ("de-sim.package" "core" "conditions" "util"))))
