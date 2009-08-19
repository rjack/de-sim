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
;(declaim (optimize (debug 0) (safety 0) speed))


(defpackage :it.unibo.cs.web.ritucci.de-sim
  (:nicknames :de-sim)
  (:use :common-lisp)
  (:export))
(in-package :de-sim)


;; errors

(define-condition not-found (error)
  nil)


(define-condition not-implemented (error)
  nil)


;; classes

(defclass dummy ()
  ((uno
    :accessor uno)
   (due
    :accessor due)))


(defclass event ()
  ((owner-id
    :accessor owner-id)
   (tm
    :accessor tm)
   (fn
    :accessor fn)))


(defclass simulator ()
  ((id
    :accessor id)
   (clock
    :accessor clock)
   (children-ht
    :accessor children-ht
    :documentation "indexed by id")
   (objects-ht
    :accessor objects-ht
    :documentation "indexed by id")))


;; utils

(defun list-slots (instance)
  (mapcar #'sb-mop::slot-definition-name
	  (sb-mop::class-slots (class-of instance))))


(defun ! (obj &rest slot-value-plist)
  "Clone obj and return the eventually modified copy.
   Example: class A has slots FOO and BAR.
            (setf *org* (make-instance 'a :foo 'foo :bar 'bar))
            (setf *cpy0* (! *org*))
            (setf *cpy1* (! *org* :foo 'new-foo))
            (setf *cpy2* (! *org* :foo 'new-foo :bar 'new-bar))"
  (let ((obj-copy (make-instance (class-of obj)))
	(slots-to-copy (list-slots obj)))
    ;; Set the given value for each slot specified in
    ;; slot-value-plist.
    (do* ((s/v slot-value-plist (cddr s/v))
	  (slot (first s/v) (first s/v))
	  (value (second s/v) (second s/v)))
	 ((null slot) nil)
      (let ((slot-symbol (intern (string slot))))
	(setf (slot-value obj-copy slot-symbol)
	      value)
	(setf slots-to-copy
	      (remove slot-symbol slots-to-copy))))
    ;; Copy the remaining slots from the original instance.
    (when (not (null slots-to-copy))
      (dolist (s slots-to-copy)
	(setf (slot-value obj-copy s)
	      (slot-value obj s))))
    obj-copy))


(defmethod fire ((sim simulator) (ev event))
  (funcall (fn ev)
	   (! sim :tm (tm ev))))


(defun id= (id1 id2)
  (= id1 id2))


(defun owner-p (sim ev)
  (id= (id sim)
       (owner-id ev)))


(defun children (sim)
  (declare (ignorable sim))
  nil)


(defun evolve (sim evs)
  "Evolve a simulator."
  (labels ((find-owner (ev &optional (sims nil))
	     "Returns the owner of ev among sims or throws a
              not-found error."
	     (let ((sim (first sims)))
	       (cond ((null sim) (error 'not-found))
		     ((owner-p sim ev) sim)
		     (t (find-owner ev (nconc (rest sims)
					      (children sim))))))))
    (let ((ev (first evs)))
      (fire (restart-case (find-owner ev sim)
	      (skip-ev ()
		(evolve sim (rest evs))))
	    ev))))
