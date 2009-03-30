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


(defpackage :org.altervista.rjack.desim
  (:nicknames :ds)
  (:use :common-lisp)
  (:export :schedule :next
	   :identifiable :id-of
	   :simulated :description-of
	   :event :time-of :action-of
	   :world :clock-of :events-of))


(in-package :ds)


(defgeneric next (world)
  (:documentation "Execute the action associated with the next event
  returning a new world, possibly different from the given one."))


(defgeneric schedule (word event)
  (:documentation "Add event into world's events."))


(defclass identifiable ()
  ((id
    :initarg :id
    :initform (error ":id missing")
    :accessor id-of
    :type fixnum
    :documentation "Unique or not, auto-generated or not, subclasses
    decide.")))


(defclass simulated (identifiable)
  ((description
    :initarg :description
    :initform "simulated object"
    :accessor description-of
    :type string
    :documentation "Description of this object.")))


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
    :documentation "Implements the behaviour of this event, taking the
    world as its only argument and modifying it.
    NOTE: function can modify the world structure only, NOT the
    structure of world's elements. This is because they are shared
    with the old instances of world that are kept in the history.")))


(defclass world (identifiable)
  ((clock
    :initform 0
    :initarg :clock
    :accessor clock-of
    :type fixnum
    :documentation "World's internal clock.")

   (events
    :initarg :events
    :initform (error "missing :events")
    :accessor events-of
    :type list
    :documentation "World's events.")))


(defmethod schedule ((w world) (ev event))
  (assert (>= (time-of ev) (clock-of w)) nil
	  "Scheduling an event in the past.")
  (setf (events-of w)
	(sort (cons ev (events-of w))
	      #'< :key #'time-of)))


(defmethod next ((w world))
  (with-accessors ((events events-of) (clock clock-of)) w
    (when (not (null events))
      (let ((ev (pop events)))
	(setf clock (time-of ev))
	(format t "~&~D " clock)
	(funcall (action-of ev) w)
	w))))
