;; DE-SIM
;; Discrete Event SIMulator.

;; Copyright (C) 2009  Giacomo Ritucci

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;   1. Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above
;;      copyright notice, this list of conditions and the following
;;      disclaimer in the documentation and/or other materials
;;      provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
;; WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.


(declaim (optimize debug safety (speed 0)))


(defpackage :org.altervista.rjack.de-sim.util
  (:nicknames :de-sim.util)
  (:use :common-lisp :de-sim.core)
  (:export :schedule
	   :size))


;; OVERVIEW

;; TODO


(in-package :de-sim.util)


 (defgeneric schedule (obj delay fn &optional args)
   (:documentation "Schedule a new event."))


 (defgeneric size (obj)
   (:documentation "Return obj's size."))


 (defmethod size ((ls list))
   (reduce #'+ ls :key #'size))


(defmethod schedule ((obj object) (delay fixnum) (fn function)
		     &optional (args nil args?))
  (if (< delay 0)
      (error "scheduling in the past")
      (setf (events-of obj)
	    (sort (cons (apply #'make-instance 'event
			       ;; if args? is nil, event's args slot
			       ;; must not be bound: suitably consing
			       ;; argument list for make-instance.
			       (cons (list :time (+ (gettime)
						    delay)
					   :fn fn)
				     (if args?
					 (list :args args)
					 nil)))
			(events-of obj))
		  #'< :key #'time-of))))
