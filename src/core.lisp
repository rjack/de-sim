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


(defpackage :org.altervista.rjack.de-sim.core
  (:nicknames :de-sim.core)
  (:use :common-lisp)
  (:export))


;; OVERVIEW

;; TODO


(in-package :de-sim.core)


(defgeneric imminent-event-time (obj)
  (:documentation "Returns time and owner of the next event to be
  executed belonging to obj."))


(defgeneric subscribe (sub obj state notification)
  (:documentation "After this call sub will be notified with the
  specified notification when obj will be in the specified state."))


(defgeneric unsubscribe (sub obj state)
  (:documentation "After this call sub will be no more notified when
  obj will be in the specified state."))


(defgeneric n-event-choosed (obj)
  (:documentation "Notify obj that its imminent event has been choosed
  for being executed."))


(defgeneric n-destroy (obj)
  (:documentation "Notify obj that it's about to be destroyed."))



(defclass identifiable ()
  ((id
    :accessor id-of
    :type fixnum
    :documentation "Unique id")))


(let ((id-counter 0))

  (defmethod initialize-instance :after ((obj identifiable) &key)
    (setf (id-of obj)
	  (incf id-counter))))




(defclass event (identifiable)
  ((time
    :initarg :time
    :initform (error ":time missing")
    :accessor time-of
    :type fixnum)
   (action
    :initarg :action
    :initform (error ":action missing")
    :accessor action-of
    :type function)))




(defclass object (identifiable)
  ((description
    :initarg :description
    :initform "simulated object"
    :accessor description-of
    :type string)
   (events
    :initform (list)
    :accessor events-of
    :type list)
   (observable-states
    :initform (list)
    :accessor observable-states-of
    :type list)
   (notifications
    :initform (list)
    :accessor notifications-of
    :type list)))


(defmethod imminent-event-time ((obj object))
  (values (time-of (first (events-of obj)))
	  obj))


(defmethod n-event-choosed ((obj object))
  (let ((ev (pop (events-of obj))))
    (declare (type event ev))
    (funcall (action-of ev) obj)))


(let ((clock 0))

  (defun gettime ()
    clock)


  (defun run-step (obj)
    (declare (type object obj))
    (multiple-value-bind (time owner)
	(imminent-event-time obj)
      (declare (type fixnum time) (type object owner))
      (setf clock time)
      (n-event-choosed owner))))
