;; DE-SIM
;; Discrete Event SIMulator.

;; Copyright (C) 2008  Giacomo Ritucci

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


(declaim (optimize debug safety (speed 0)))

(defpackage :org.altervista.rjack.desim
  (:nicknames :ds)
  (:use :common-lisp)
  (:export :event
	   :world
	   :run))

(in-package :ds)


(defclass identifiable ()
  ((id
    :initarg :id
    :initform (error ":id missing")
    :reader id-of
    :type fixnum
    :documentation "Unique or not, subclasses decide.")))


(defclass event (identifiable)
  ((time
    :initarg :time
    :initform (error "missing :time")
    :reader time-of
    :type fixnum
    :documentation "When this event must be executed.")

   (action
    :initarg :action
    :initform (error "missing :action")
    :reader action-of
    :type function   ; FIXME: must fully specify signature.
    :documentation "Implements the behaviour of this event.")))


(defclass world (identifiable)
  ((history
    :initform ()
    :accessor history-of
    :type list
    :documentation "Past instances of this world, newer first.")

   (events
    :initarg events
    :initform ()
    :accessor events-of
    :type list
    :documentation "World's events.")))


(defmethod initialize-instance :after ((w world) &key)
  (push w (history-of w)))


(defgeneric run (world event)
  (:documentation "Execute the action associated with event returning
  a new world, possibly different from the given one."))


(defgeneric next (world)
  (:documentation "Return the next world, based on the given one."))


(defgeneric prev (world)
  (:documentation "Return the previous world, relative to the given
  one."))


(defmethod run ((w world) (ev event))
  (let* ((new-world (copy-structure w))
	 (this (pop (events-of new-world))))
    (push new-world (history-of new-world))
    (assert (eql this ev) nil
	    "executing event should be the first in the event list.")
    (funcall (action-of ev) new-world)
    new-world))


(defmethod next ((w world))
  (run w (first (events-of w))))


(defmethod prev ((w world))
  (second (history-of w)))
