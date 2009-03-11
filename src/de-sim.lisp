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

(in-package :org.altervista.rjack.desim)


(defclass identifiable ()
  ((id
    :initarg :id
    :initform (error ":id missing")
    :reader id-of
    :type fixnum
    :documentation "Identifier. Can be unique or not, subclasses decide.")))


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



(defclass world ()
  ((events
    :initarg events
    :initform ()
    :accessor events-of
    :type list
    :documentation "World's events.")))


(defgeneric run (context obj)
  (:documentation "Execute the action associated with the object in
  the given context."))


(defmethod run ((w world) (ev event))
  (funcall (action-of ev) w))
