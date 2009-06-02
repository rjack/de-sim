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


(define-test collect-list
  (let* ((size (random 1000))
	 (lst (collect-list size (lambda ()
				   (make-instance 'object)))))
    (assert-eql size (length lst))))


(define-test schedule
  (let ((number-of-events 1000))
    (let ((obj (make-instance 'object))
	  (times (collect-list number-of-events (lambda ()
						  (random 100))))
	  (functs (collect-list number-of-events (lambda ()
						   (if (< (random 100)
							  50)
						       #'null
						       #'zerop)))))
      ;; must reject negative delays (scheduling in the past)
      (assert-error 'error (schedule obj -1 #'null))

      ;; schedule events
      (mapc (lambda (time fun)
	      (schedule obj time fun))
	    times functs)

      ;; must order events ordered by time, ascending
      (assert-true (apply #'<= (mapcar #'time-of (events-of obj)))
		   (mapcar #'time-of (events-of obj)))
      ;; must be able to find what has been inserted

      (let ((null-evs (remove-if (lambda (x)
				   (not (eql x #'null)))
				 (events-of obj) :key #'fn-of))
	    (zerop-evs (remove-if (lambda (x)
				    (not (eql x #'zerop)))
				  (events-of obj) :key #'fn-of)))
	(assert-equal number-of-events
		      (+ (length null-evs)
			 (length zerop-evs))))

      ;; args-of must be unbound if not specified as parameter of schedule
      (schedule obj 100 #'car)
      (let ((ev (find #'car (events-of obj) :key #'fn-of)))
	(assert-false (slot-boundp ev 'de-sim::args)))

      (schedule obj 100 #'cdr '(foo-arg baz-arg))
      (let ((ev (find #'cdr (events-of obj) :key #'fn-of)))
	(assert-true (slot-boundp ev 'de-sim::args))))))


(define-test will?
  (labels ((good-fn () nil)
	   (bad-fn () nil))
    (let ((obj (make-instance 'object))
	  (good-time 10)
	  (bad-time 11))
      (schedule obj good-time #'good-fn)
      (assert-true (will? obj))
      (assert-true (will? obj :at good-time))
      (assert-false (will? obj :at bad-time))
      (assert-true (will? obj :do #'good-fn))
      (assert-false (will? obj :do #'bad-fn))
      (assert-true (will? obj :at good-time :do #'good-fn))
      (assert-false (will? obj :at bad-time :do #'good-fn))
      (assert-false (will? obj :at good-time :do #'bad-fn))
      ; next will return false because args is unbound
      (assert-false (will? obj
			   :at good-time
			   :do #'good-fn
			   :with (lambda (args)
				   (declare (ignore args))
				   t)))
      ; clear event
      (setf (events-of obj)
	    nil)

      ; new event with args
      (schedule obj good-time #'good-fn 0 1 2 3 4 5)

      (assert-true (will? obj
			  :at good-time
			  :do #'good-fn
			  :with (lambda (args)
				  (= (nth 3 args)
				     3))))
      (assert-false (will? obj
			   :at good-time
			   :do #'good-fn
			   :with (lambda (args)
				   (= (nth 3 args)
				      0)))))))


(run-tests)
