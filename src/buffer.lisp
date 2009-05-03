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
  (:use :common-lisp :de-sim.core)
  (:export))


;; OVERVIEW

;; TODO


(in-package :de-sim.buffer)




(defclass buffer (object)
  ((de-sim.core:description
    :initform "buffer")
   (de-sim.core:notifications
    :initform (list #'n-inserted
		    #'n-removed
		    #'n-bandwidth-changed
		    #'n-input-started
		    #'n-input-ended
		    #'n-output-started
		    #'n-output-ended))
   (incoming-bandwidth
    :initarg :incoming-bandwidth
    :initform -1
    :accessor incoming-bandwidth-of
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


(defmethod size ((buf buffer))
  (reduce #'+ (elements-of buf) :key (size-fn-of buf)))


(defmethod subscribable-states ((buf buffer))
  (list :item-inserted
	:item-removed
	:buffer-full
	:buffer-empty))


(defmethod n-inserted ((buf buffer) (new object))
  (if (> (+ (funcall (size-fn-of buf) new)
	    (size buf))
	 (capacity-of buf))
      (error "new object does not fit")
      (progn
	(setf (elements-of buf)
	      (append (elements-of buf)
		      (list new)))
	(notify-subscribed buf :item-inserted)
	(when (= (size buf)
		 (capacity-of buf))
	  (notify-subscribed buf :buffer-full)))))


(defmethod n-removed ((buf buffer))
  (if (null (elements-of buf))
      (error "cannot remove, buffer empty")
      (let ((popped (pop (elements-of buf))))
	(notify-subscribed buf :item-removed)
	(when (zerop (size buf))
	  (notify-subscribed buf :buffer-empty))
	popped)))


(defmethod n-bandwidth-changed ((buf buffer) (bw fixnum))
  nil)


(defmethod n-input-started ((buf buffer) (incoming object))
  nil)


(defmethod n-input-ended ((buf buffer) (incoming object))
  nil)


(defmethod n-output-started ((buf buffer) (outgoing object))
  nil)


(defmethod n-output-ended ((buf buffer) (outgoing object))
  nil)