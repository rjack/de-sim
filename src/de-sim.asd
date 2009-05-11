(defpackage :de-sim.asd
  (:use :cl :asdf))

(in-package :de-sim.asd)


(defsystem de-sim
    :name "core"
    :author "Giacomo Ritucci"
    :version "0.1"
    :license "2 clauses BSD style, see COPYING file for details"
    :components ((:file "core")
		 (:file "util")
		 (:file "buffer"
			:depends-on ("core" "util"))))