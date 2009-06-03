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


(in-package :de-sim.test)


(defun fresh-actor ()
  (make-instance 'actor :id (fresh-id)))


(defun fresh-object ()
  (make-instance 'object :id (fresh-id)))


(define-test event-instance
  (let ((ev (make-instance 'event :id 42 :time 123 :fn #'null)))
    (assert-eql 42 (id-of ev))
    (assert-eql 123 (time-of ev))
    (assert-eql #'null (fn-of ev))))


(define-test object-instance
  (let ((obj (make-instance 'object :id 42)))
    (assert-eql 42 (id-of obj))))


(define-test actor-instance
  (let ((act (make-instance 'actor :id 42)))
    (assert-eql 42 (id-of act))
    (assert-true (null (events-of act)))))


(define-test simulator-instance
  (let ((sim (make-instance 'simulator :id 42)))
    (assert-eql 42 (id-of sim))
    (assert-true (null (events-of sim)))
    (assert-true (null (in-ports-of sim)))
    (assert-true (null (out-ports-of sim)))
    (assert-true (null (components-of sim)))))


(define-test schedule-object
  (let ((obj (make-instance 'object :id 42)))
    ;; there is no schedule method specialized on object
    (assert-error 'simple-error (schedule obj #'null))))


(define-test schedule-actor
  (let ((fn #'null)
	(small-at 0)
	(medium-at 5)
	(big-at 10)
	(bad-at -1))
    ;; no negative time
    (assert-error 'error (schedule (fresh-actor) fn :at bad-at))

    (let ((act (schedule (schedule (schedule (fresh-actor) fn :at
					     big-at)
				   fn :at medium-at)
			 fn :at small-at)))
      (assert-eql small-at (time-of (first (events-of act))))
      (assert-eql medium-at (time-of (second (events-of act))))
      (assert-eql big-at (time-of (third (events-of act)))))))


(define-test imminent-event-object
  (assert-true (null (imminent-event (fresh-object)))))


(define-test imminent-event-actor
  (let ((act (fresh-actor)))
    ;; should have no events
    (assert-true (null (imminent-event act)))
    (let ((act (schedule act #'null :at 10 :id 0)))
      ;; should return the event
      (assert-eql 0 (id-of (imminent-event act)))
      ;; max time 5, event at 10, should return nil
      (assert-true (null (imminent-event act 5)))
      (let ((act (schedule act #'null :at 5 :id 1)))
	(assert-eql 1 (id-of (imminent-event act 5)))))))


(define-test imminent-event-simulator
  (let ((sim (schedule (make-instance 'simulator :id (fresh-id)
				      :components (list (schedule (fresh-actor) #'null :at 10 :id 4)
							(fresh-object)
							(schedule (make-instance 'simulator :id (fresh-id)
										 :components (list (fresh-object)
												   (schedule (fresh-actor) #'null :at 0 :id 1)))
								  #'null :at 5 :id 2)))
		       #'null :at 4 :id 3)))
    (assert-eql 1 (id-of (imminent-event sim)))))


(define-test i/o-connect-out-in
  (let ((out (make-instance 'out-port :id (fresh-id)))
	(in (make-instance 'in-port :id (fresh-id))))
    (i/o-connect out in)
    (assert-true (i/o-connected-p out in))))


(define-test i/o-connect-in-act
  (let ((in (make-instance 'in-port :id (fresh-id)))
	(act (make-instance 'actor :id (fresh-id))))
    (i/o-connect in act)
    (assert-true (i/o-connected-p in act))))
