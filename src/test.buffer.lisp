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
  (:use :common-lisp :lisp-unit :de-sim.core :de-sim.util :de-sim.buffer))

(in-package :de-sim.test.buffer)




(define-test buffer-class
  (let ((buf (make-instance 'buffer))
	(items (collect-list 100 (lambda ()
				   (make-instance 'object)))))
    ;; subcribable states
    (assert-equal (list :input :output :full :empty)
		  (subscribable-states-of buf))
    ;; notifications
    (assert-equal (list #'n-input #'n-output)
		  (notifications-of buf))

    ;; should be empty
    (assert-true (empty? buf))

    ;; should be not full
    (assert-false (full? buf))

    ;; input
    (dolist (i items)
      (n-input buf i))
    (assert-equal items (elements-of buf))

    ;; no more empty
    (assert-false (empty? buf))

    ;; but never full
    ;; (can't prove that will be *never* full, you have to trust it).
    (assert-false (full? buf))))




(run-tests)