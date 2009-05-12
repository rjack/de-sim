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


(defpackage :org.altervista.rjack.de-sim.test.buffer
  (:nicknames :de-sim.test.buffer)
  (:use :common-lisp :lisp-unit
	:de-sim.core :de-sim.util :de-sim.buffer :de-sim.conditions))

(in-package :de-sim.test.buffer)




(defparameter *observer* (make-instance 'object))
(defparameter *buf* (make-instance 'buffer))
(defparameter *input-count* 0)
(defparameter *output-count* 0)
(defparameter *items* (collect-list 100 (lambda ()
					  (make-instance 'object))))




(defun input-occurred (obj)
  (declare (ignore obj))
  (incf *input-count*))


(defun output-occurred (obj)
  (declare (ignore obj))
  (incf *output-count*))




(define-test buffer-class

  ;; subcribable states
  (assert-equal (list :input :output :full :empty)
		(subscribable-states-of *buf*))
  ;; notifications
  (assert-equal (list #'n-input #'n-output)
		(notifications-of *buf*))

  ;; should be empty
  (assert-true (empty? *buf*))

  ;; should be not full
  (assert-false (full? *buf*)))




(define-test buffer-subscribe

  ;; observer wants to know when input and output occurr.
  (assert-true (subscribe *observer* *buf* :input #'input-occurred))
  (assert-true (subscribe *observer* *buf* :output #'output-occurred))

  ;; subscribing again should raise an error
  (assert-error 'error (subscribe *observer* *buf* :input #'input-occurred))
  (assert-error 'error (subscribe *observer* *buf* :output #'output-occurred)))




(define-test buffer-input

  (setf *input-count* 0)
  (setf *output-count* 0)

  ;; insert all items
  (dolist (i *items*)
    (n-input *buf* i))

  ;; same list
  (assert-equal *items* (elements-of *buf*))
  ;; all notified?
  (assert-equal (length *items*) *input-count*)
  ;; paranoid
  (assert-equal 0 *output-count*)

  ;; no more empty
  (assert-false (empty? *buf*))

  ;; but never full
  ;; (can't prove that will be *never* full).
  (assert-false (full? *buf*)))



;; NOTE: run after buffer-input
(define-test buffer-output

  (let ((removed-items (list)))
    (dotimes (i (length *items*))
      (setf removed-items
	    (append removed-items
		    (list (n-output *buf*)))))

    ;; now should be empty
    (assert-true (empty? *buf*))
    ;; let's raise an error, just to be sure
    (assert-error 'error-empty (n-output *buf*))
    ;; and should not be full (yes, paranoid)
    (assert-false (full? *buf*))
    ;; all notified?
    (assert-equal (length *items*) *output-count*)
    ;; removed list is the original one?
    (assert-equal *items* removed-items)))




(run-tests buffer-class buffer-subscribe buffer-input buffer-output)
