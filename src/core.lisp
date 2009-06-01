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


;; OVERVIEW

;; TODO


(in-package :de-sim)


(defgeneric evolve (obj now)
  (:documentation "obj can execute its event and evolve
  accordingly. Returns the new version of obj."))


(defgeneric imminent-event (obj)
  (:documentation "Return the time of the next event of obj."))


(defgeneric i/o-connect (output input)
  (:documentation "Connect output to input."))


(defgeneric i/o-connected-p (output input)
  (:documentation "Return t if output is connected to input, nil
  otherwise"))


(defgeneric i/o-disconnect (output input)
  (:documentation "Reverse the effect of i/o-connect."))


(deftype id-type ()
  '(integer 0))


(deftype time-type ()
  '(or (integer 0)
    (member :never)))


(defclass with-id ()
  ((id
    :initform (error ":id missing")
    :reader id-of
    :type id-type
    :documentation "Unique id")))


(defclass event (with-id)
  ((time
    :initarg :time
    :initform (error ":time missing")
    :accessor time-of
    :type time-type)
   (fn
    :initarg :fn
    :initform (error ":action missing")
    :accessor fn-of
    :type function))


(defclass with-behaviour ()
  ((events
    :initform (list)
    :accessor events-of
    :type list)))


(defclass object (with-id)
  nil)



(defmethod imminent-event ((obj object))
  (values :never nil))



(defmethod imminent-event ((obj with-behaviour))
  (let ((ev (first (events-of obj))))
    (values (time-of ev) (fn-of ev))))


(defmethod evolve ((obj object)))