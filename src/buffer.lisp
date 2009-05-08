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


(defpackage :org.altervista.rjack.de-sim.buffer
  (:nicknames :de-sim.buffer)
  (:use :common-lisp :de-sim.core :de-sim.util)
  (:export))


;; OVERVIEW

;; Public notifications (other objects can use these notifications on
;; buffer)

; n-input-started
; n-input-ended
; n-output-started
; n-output-ended

;; Private notifications

; n-input-should-end
; n-output-should-end


(in-package :de-sim.buffer)


(defgeneric fits? (buf obj)
  (:documentation "Return T if buf has enough room to accomodate obj,
  NIL otherwise"))


(defgeneric full? (buf)
  (:documentation "Return T if buf is full."))


(defgeneric empty? (buf)
  (:documentation "Return T if buf is empty."))


(defgeneric n-inserted (buf new)
  (:documentation "Notifies the insertion of new into buf."))


(defgeneric n-removed (buf)
  (:documentation "Notifies the removal of the first element of
  buf.
  Returns: the removed element.
  Signals: error-empty."))


(defgeneric n-input-bandwidth-changed (buf new-bw)
  (:documentation "Notifies that buf's new input bandwidth has the new
  value new-bw.
  Returns: nil
  Signals: error-bad-value."))


(defgeneric n-output-bandwidth-changed (buf new-bw)
  (:documentation "Notifies that buf's new output bandwidth has the
  new value new-bw.
  Returns: nil
  Signals: error-bad-value."))


(defgeneric n-input-started (buf obj)
  (:documentation "Notifies that the input process of obj into buf has
  started."))



(defclass buffer (object)
  ((de-sim.core:description
    :initform "buffer")
   (de-sim.core:notifications
    :initform (list #'n-inserted
		    #'n-removed
		    #'n-input-bandwidth-changed
		    #'n-output-bandwidth-changed
		    #'n-input-started
		    #'n-input-ended
		    #'n-output-started
		    #'n-output-ended))
   (input-bandwidth
    :initarg :input-bandwidth
    :initform -1
    :accessor input-bandwidth-of
    :type fixnum
    :documentation "Negative means infinite")
   (outgoing-bandwidth
    :initarg :outgoing-bandwidth
    :initform -1
    :accessor outgoing-bandwidth-of
    :type fixnum
    :documentation "Negative means infinite")
   (elements
    :initform (list)
    :accessor elements-of
    :type list)
   (outgoing-element
    :initform nil
    :accessor outgoing-element-of
    :type object)
   (incoming-element
    :initform nil
    :accessor incoming-element-of
    :type object)
   (size-fn
    :initarg :size-fn
    :initform (error ":size-fn missing")
    :accessor size-fn-of
    :type function)
   (capacity
    :initform -1
    :accessor capacity-of
    :type fixnum
    :documentation "Negative means infinite")))


(defmethod fits? ((buf buffer) (obj object))
  (> (capacity-of buf)
     (+ (size obj)
	(size (elements-of buf)))))


(defmethod full? ((buf buffer))
  (= (size (elements-of buf))
     (capacity-of buf)))


(defmethod empty? ((buf buffer))
  (zerop (size (elements-of buf))))


(defmethod subscribable-states ((buf buffer))
  (list :item-inserted
	:item-removed
	:buffer-full
	:buffer-empty))


(defmethod n-inserted ((buf buffer) (new object))
  (if (not (fits? buf new))
      (error "new object does not fit")
      (progn
	(setf (elements-of buf)
	      (append (elements-of buf)
		      (list new)))
	(notify-subscribed buf :item-inserted)
	(when
	  (notify-subscribed buf :buffer-full)))))


(defmethod n-removed ((buf buffer))
  (if (null (elements-of buf))
      (error "cannot remove, buffer empty")
      (let ((popped (pop (elements-of buf))))
	(notify-subscribed buf :item-removed)
	(when (zerop (size buf))
	  (notify-subscribed buf :buffer-empty))
	popped)))


(defmethod n-input-bandwidth-changed ((buf buffer) (bw fixnum))
  nil)


(defmethod n-output-bandwidth-changed ((buf buffer) (bw fixnum))
  nil)


(defmethod n-input-started ((buf buffer) (incoming object))
  (if (not (null (incoming-element-of buf)))
      (error "input busy")
      (let ((delay (/ (size buf)
		      (input-bandwidth-of buf))))
	(setf (incoming-element-of buf)
	      incoming)
	(schedule buf delay #'n-input-should-end incoming))))


(defmethod n-input-should-end ((buf buffer) (incoming object))
  (when (and (eql incoming (incoming-element-of buf))
	     (or (will? buf
			:at (gettime)
			:do #'n-input-ended
			:with (lambda (args)
				(eql (second args)
				     incoming)))))
	  (error "input should have ended!")))



(defmethod n-input-ended ((buf buffer) (incoming object))
  (if (not (eql incoming
		(incoming-element-of buf)))
      (error "input-ended error")
      (progn
	(setf (elements-of buf)
	      (append (elements-of buf)
		      (list incoming)))
	(setf (incoming-element-of buf)
	      nil))))


(defmethod n-output-started ((buf buffer) (outgoing object))
  nil)


(defmethod n-output-ended ((buf buffer) (outgoing object))
  nil)
