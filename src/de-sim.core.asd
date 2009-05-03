(defpackage :asd.core
  (:use :cl :asdf))

(in-package :asd.core)


(defsystem de-sim.core
    :name "core"
    :author "Giacomo Ritucci"
    :version "0.1"
    :license "2 clauses BSD style, see COPYING file for details"
    :components ((:file "core")
		 (:file "buffer"
			:depends-on ("core"))))
