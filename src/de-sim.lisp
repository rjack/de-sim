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
  (:export :!transform :clone :run :next :prev
	   :identifiable :id-of
	   :simulated :description-of
	   :event :time-of :!action-of
	   :world :hystory-of :events-of))


(in-package :ds)


(defgeneric !transform (dest source)
  (:documentation "Fill dest with some, all or none of the values from
  source."))


(defgeneric clone (object)
  (:documentation "Return a shallow copy of object."))


(defgeneric run (world event)
  (:documentation "Execute the action associated with event returning
  a new world, possibly different from the given one."))


(defgeneric next (world)
  (:documentation "Return the next world, based on the given one."))


(defgeneric prev (world)
  (:documentation "Return the previous world, relative to the given
  one."))


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

   (!action
    :initarg :!action
    :initform (error "missing :action")
    :reader !action-of
    :type function   ; FIXME: must fully specify signature.
    :documentation "Implements the behaviour of this event, taking the
    world as its only argument and modifying it.
    NOTE: function can modify the world structure only, NOT the
    structure of world's elements. This is because they are shared
    with the old instances of world that are kept in the history.")))


(defclass world (identifiable)
  ((history
    :initform ()
    :accessor history-of
    :type list
    :documentation "Past instances of this world, newer first.")

   (events
    :initarg :events
    :initform (error "missing :events")
    :accessor events-of
    :type list
    :documentation "World's events.")))


(defmethod initialize-instance :after ((w world) &key)
  (push w (history-of w)))


(defmethod !transform ((dest world) (source world))
  (setf (events-of dest) (events-of source))
  (setf (history-of dest) (history-of source))
  dest)


(defmethod clone ((w world))
  (!transform (make-instance 'world :id (id-of w) :events nil) w))


(defmethod run :around ((w world) (ev event))
  (format t "~&~D " (time-of ev))
  (call-next-method))


(defmethod run ((w world) (ev event))
  (let* ((new-world (clone w))
	 (this (pop (events-of new-world))))
    (push new-world (history-of new-world))
    (assert (eql this ev) nil
	    "executing event should be the first in the event list.")
    (funcall (!action-of ev) new-world)
    new-world))


(defmethod next ((w world))
  (run w (first (events-of w))))


(defmethod prev ((w world))
  (second (history-of w)))
