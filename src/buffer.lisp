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


(in-package :de-sim)


;; OVERVIEW

;; This is a simple FIFO buffer with unlimited capacity.

;; Subscribable states:
;;
;; :input      item is inserted
;; :output     item is removed
;; :empty      last item is removed
;; :full       item is inserted and no more can fit

;; This buffer never notifies :full, subclasses may do.


(defgeneric fits? (buf obj)
  (:documentation "Return T if buf has enough room to accomodate obj,
  NIL otherwise"))


(defgeneric full? (buf)
  (:documentation "Return T if buf is full."))


(defgeneric empty? (buf)
  (:documentation "Return T if buf is empty."))


(defgeneric n-input (buf obj)
  (:documentation "Notifies that buf has received obj."))


(defgeneric n-output (buf)
  (:documentation "Notifies that buf should perform output."))




(defclass buffer (object)
  ((description
    :initform "buffer")
   (notifications
    :initform (list #'n-input
		    #'n-output))
   (subscribable-states
    :initform (list :input
		    :output
		    :full
		    :empty))
   (elements
    :initform (list)
    :accessor elements-of
    :type list)))




(defmethod fits? ((buf buffer) (obj object))
  t)


(defmethod full? ((buf buffer))
  nil)


(defmethod empty? ((buf buffer))
  (null (elements-of buf)))


(defmethod n-input ((buf buffer) (new object))
  (if (not (fits? buf new))
      (error 'error-full)
      (progn
	(setf (elements-of buf)
	      (append (elements-of buf)
		      (list new)))
	(notify-subscribed buf :input)
	(when (full? buf)
	  (notify-subscribed buf :full)))))


(defmethod n-output ((buf buffer))
  (if (null (elements-of buf))
      (error 'error-empty)
      (let ((popped (pop (elements-of buf))))
	(notify-subscribed buf :output)
	(when (empty? buf)
	  (notify-subscribed buf :empty))
	popped)))
