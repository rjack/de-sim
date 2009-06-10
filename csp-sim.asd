(defpackage :csp-sim.asd
  (:use :cl :asdf))

(in-package :csp-sim.asd)


(defsystem csp-sim
    :name "csp-sim"
    :author "Giacomo Ritucci"
    :version "0.1"
    :license "2 clauses BSD style, see COPYING file for details"
    :depends-on ("de-sim")
    :components ((:file "csp-sim")))
