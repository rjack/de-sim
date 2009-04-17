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


(defpackage :org.altervista.rjack.de-sim.test.core
  (:nicknames :de-sim.test.core)
  (:use :common-lisp :lisp-unit :de-sim.core))


(in-package :de-sim.test.core)


(defparameter *times* 1000)


(define-test world-make-instance
    (let ((w (make-instance 'world :id 0 :clock 100 :events nil)))
      (assert-true (typep w 'world))
      (assert-eql 0 (id-of w))
      (assert-eql 100 (clock-of w))
      (assert-eql nil (events-of w))))


(define-test event-make-instance
    (labels ((action-foo ()
	       nil))
      (let ((ev (make-instance 'event :id 0 :time 12 :action #'action-foo)))
	(assert-true (typep ev 'event))
	(assert-eql 0 (id-of ev))
	(assert-eql 12 (time-of ev))
	(assert-eql #'action-foo (action-of ev)))))


(define-test schedule
    (let ((w (make-instance 'world :id 0 :events nil)))
      (dotimes (i *times*)
	(assert-eql i (length (events-of w)))
	(schedule w (make-instance 'event :id i :time (random 1000) :action #'break))
	(assert-eql (1+ i) (length (events-of w)))
	(assert-true (apply #'<= (map 'list #'time-of (events-of w)))))))


(define-test next
    (with-open-file (*standard-output* "/dev/null" :direction :output :if-exists :overwrite)
      (let ((w (make-instance 'world :id 0 :events nil))
	    (clock -1)
	    (count 0))
	(labels ((action (world)
		   (incf count)
		   (setf clock (clock-of world)))
		 (run-all (world)
		   (when (not (zerop (length (events-of world))))
		     (let ((new-world (next w)))
		       (assert-eql clock (clock-of new-world))
		       (run-all new-world)))))
	  (dotimes (i *times*)
	    (schedule w (make-instance 'event :id i :time i :action #'action)))
	  (run-all w)
	  (assert-eql *times* count)))))


(run-tests)
