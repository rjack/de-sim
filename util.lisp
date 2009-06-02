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


;; OVERVIEW

;; TODO


(in-package :de-sim)


(defun collect-list (size fun)
  (declare (type fixnum size) (type function fun))
  (labels ((collect-list-tr (i size fun lst)
	     (if (< i size)
		 (collect-list-tr (1+ i) size fun (cons (funcall fun)
							lst))
		 (nreverse lst))))
    (collect-list-tr 0 size fun (list))))




(defgeneric schedule (obj delay fn &rest args)
  (:documentation "Schedule a new event."))


(defgeneric size (obj)
  (:documentation "Return obj's size."))


(defgeneric will? (obj &key at do with))




(defmethod size ((ls list))
  (reduce #'+ ls :key #'size))


(defmethod schedule ((obj object) (delay fixnum) (fn function)
		     &rest args)
  (if (< delay 0)
      (error 'error-invalid)
      (setf (events-of obj)
	    (sort (cons (apply #'make-instance 'event
			       ;; if args? is nil, event's args slot
			       ;; must not be bound: suitably consing
			       ;; argument list for make-instance.
			       (append (list :time (+ (gettime)
						      delay)
					     :fn fn)
				       (if (null args)
					   nil
					   (list :args args))))
			(events-of obj))
		  #'< :key #'time-of))))


(defmethod will? ((obj object) &key (at -1 at?) (do #'null do?) (with #'null with?))
  (declare (type fixnum at)
	   (type function do with))
  (find-if (lambda (ev)
	     (and (or (not at?)
		      (= at
			 (time-of ev)))
		  (or (not do?)
		      (eql do
			   (fn-of ev)))
		  (or (not with?)
		      (and (slot-boundp ev 'args)
			   (funcall with (args-of ev))))))
	   (events-of obj)))
