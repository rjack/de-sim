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
  (:export :imminent-event-time
	   :subscribable-states :subscribed? :subscribe :unsubscribe
	   :n-event-choosed :n-destroy
	   :id-of
	   :event :time-of :action-of
	   :object :description :description-of :events-of
	           :subscriptions-of :notifications :notifications-of
		   :notify-subscribed
	   :gettime :run-step))


;; OVERVIEW

;; TODO


(in-package :de-sim.core)


(defgeneric imminent-event-time (obj)
  (:documentation "Return time and owner of the next event to be
  executed belonging to obj."))


(defgeneric subscribable-states (obj)
  (:documentation "Return the list of subscribable states of obj."))


(defgeneric subscribe (sub obj state notification)
  (:documentation "After this call sub will be notified with the
  specified notification when obj will be in the specified state."))


(defgeneric subscribed? (sub obj state)
  (:documentation "Return t if sub is subscribed to obj's state, nil
  otherwise"))


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


(let ((id-counter 0))

  (defmethod initialize-instance :after ((ev event) &key)
    (setf (id-of ev)
	  (incf id-counter))))




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
   (subscriptions
    :initform (make-hash-table)
    :reader subscriptions-of)
   (notifications
    :initform (list)
    :accessor notifications-of
    :type list)))


(let ((id-counter 0))

    (defmethod initialize-instance :after ((obj object) &key)
      (setf (id-of obj)
	    (incf id-counter))
      (dolist (ss (subscribable-states obj))
	(setf (gethash ss (subscriptions-of obj))
	      (list)))))


(defmethod imminent-event-time ((obj object))
  (values (time-of (first (events-of obj)))
	  obj))


(defmethod n-event-choosed ((obj object))
  (let ((ev (pop (events-of obj))))
    (declare (type event ev))
    (funcall (action-of ev) obj)))


(defmethod n-destroy ((obj object))
  nil)


(defmethod subscribable-states ((obj object))
  (list))


(defmethod subscribed? ((sub object) (obj object) state)
  (multiple-value-bind (subscribers state-present?)
      (gethash state (subscriptions-of obj))
    (if (not state-present?)
	(error "not a valid state")
	(not (null (find sub subscribers :key #'car))))))


(defmethod notify-subscribed ((obj object) state)
  (multiple-value-bind (subscribers state-present?)
      (gethash state (subscriptions-of obj))
    (if (not state-present?)
	(error "not a valid state")
	(dolist (sub subscribers)
	  (funcall (cdr sub)
		   (car sub))))))


(defmethod subscribe ((sub object) (obj object) state notification)
  (if (subscribed? sub obj state)
      (error "already subscribed to state")
      (not (null (push `(,sub . ,notification)
		       (gethash state
				(subscriptions-of obj)))))))


(defmethod unsubscribe ((sub object) (obj object) state)
  (if (not (subscribed? sub obj state))
      (error "not suscribed to state")
      (setf (gethash state
		     (subscriptions-of obj))
	    (delete sub (gethash state
				 (subscriptions-of obj))
		    :key #'car))))




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
