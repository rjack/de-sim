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
;(declaim (optimize (debug 0) (safety 0) speed))


;; OVERVIEW

;; TODO

(defpackage :org.altervista.rjack.de-sim
  (:nicknames :de-sim)
  (:use :common-lisp)
  (:export
   TODO)
(in-package :de-sim)



;; GENERICS



;; TYPES

(deftype id-type ()
  '(integer 0))


(deftype time-type ()
  '(integer 0))



;; CLASSES


(defclass object ()
  ;; TODO: merge object's slot in simulator
  ;; and remove object/simulator distinction
  ((id
    :reader id
    :type id-type)
   (parent
    :accessor parent)))


(defclass simulator (object)
  ((clock
    :accessor clock
    :type time-type)
   (children-by-id
    :initform (make-hash-table)
    :reader children-by-id
    :type hash-table)))


(defclass scenario (simulator)
  ((events
    :accessor events
    :initform (list)
    :type list)))


(defclass event ()
  ((canceled-p
    :initform nil
    :accessor canceled-p
    :type boolean)
   (id
    :reader id
    :type id-type)
   (owner
    :initarg :owner
    :initform (error ":owner missing")
    :accessor owner
    :type simulator)
   (time
    :initarg :time
    :initform (error ":time missing")
    :accessor time
    :type time-type)
   (fn
    :initarg :fn
    :initform (error ":fn missing")
    :accessor fn
    :type function)
   (args
    :initarg :args
    :accessor args
    :type list
    :documentation "unbound if unused")))


(defclass channel ()
  ((id
    :reader id
    :type id-type)
   (peers
    :accessor peers
    :type (cons (or nil simulator) (or nil simulator)))
   (locks
    :accessor locks
    :type (cons boolean boolean))
   (waiting-no
    :initform (cons 0 0)
    :accessor waiting-no
    :type (cons (integer 0) (integer 0)))))


;; CONDITIONS AND ERRORS

(define-condition channel-i/o-error (error)
  nil)


(define-condition channel-not-connected (channel-i/o-error)
  nil)


(define-condition channel-locked (channel-i/o-error)
  nil)



;; RESTART FUNCTIONS

(defun wait (c)
  (cond ((typep c 'channel-locked) (invoke-restart 'wait))
	(t (error "Bad condition type!"))))


(defun cancel (c)
  (declare (ignore c))
  (invoke-restart 'cancel))


;; INITIALIZE-INSTANCE

(let ((fresh-channel-id 0))
  (defmethod initialize-instance :after ((ch channel) &key)
    (setf (slot-value ch 'id)
	  (incf fresh-channel-id))))


(let ((fresh-event-id 0))
  (defmethod initialize-instance :after ((ev event) &key)
    (setf (slot-value ev 'id)
	  (incf fresh-event-id))))


(let ((fresh-object-id 0))
  (defmethod initialize-instance :after ((obj object) &key)
    (setf (slot-value obj 'id)
	  (incf fresh-object-id))))



;; PRINT-OBJECT

(defmethod print-object ((ev event) s)
  (print-unreadable-object (ev s :type t)
    (format s "time: ~a fn: ~a"
	    (time ev) (fn ev))
    (when (slot-boundp ev 'args)
      (format s " args: ~a" (args ev)))
    (format s "owner: ~a" (owner ev))))



;; FUNCTIONS AND METHODS


(defmethod same? ((a object) (b object))
  "Contract: object object -> boolean

   Purpose: to test for equality."
  (eql (id a)
       (id b)))


(defmethod input-evs ((sim simulator) (ch channel) (obj object))
  "Contract: simulator channel object -> events

   Purpose: to add object to the children of sim.

   Note: specializing methods must call-next-method."
  (add-child sim obj))


(defmethod connect! ((sim-1 simulator) (sim-2 simulator))
  "Contract: simulator out-port in-port -> nil"
  (setf (peer-port out) in)
  (setf (peer-port in) out)
  nil)


(defmethod disconnect-port ((p port))
  "Contract: out-port -> nil"
  (slot-unbound 'port (owner (peer-port p)) 'peer-port)
  (slot-unbound 'port p 'peer-port)
  nil)


(defmethod lock-evs ((sim simulator) (p port) (obj object))
  "Contract: simulator port object -> events

   Purpose: to lock the port and decide when unlock it.

   Stereotype: Template Method for access-port.

   Description: the default lock policy is to not lock.
                Specialize this method to provide your own lock
                policy."
  (assert (not (lock p)) nil "Locking a locked port.")
  nil)


(defmethod unlock-evs :before ((sim simulator) (p port))
  (assert (lock p) nil "Unlocking a not locked port!")
  (setf (lock p) nil))


(defmethod unlock-evs ((sim simulator) (in in-port))
  "Contract: out-port -> events"
  (let ((waiting (pop (waiting-queue in))))
    (when waiting
      (list (make-instance 'event
			   :owner waiting
			   :time (clock sim)
			   :fn #'in-ready-evs
			   :args (list (peer-port in)))))))


(defmethod unlock-evs ((sim simulator) (out out-port))
  "Contract: out-port -> events"
  (list (make-instance 'event
		       :owner sim
		       :time (clock sim)
		       :fn #'out-ready-evs
		       :args (list out))))


(defmethod in-ready-evs ((sim simulator) (out out-port))
  "Contract: simulator port -> events

   Description: specialize this method to generate events when the
                port connected to out gets unlocked."
  nil)


(defmethod out-ready-evs ((sim simulator) (out out-port))
  "Contract: simulator port -> events

   Description: specialize this method to generate events when out
                gets unlocked."
  nil)


(defmethod access-port ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> (or in-port nil)
                                          events

   Purpose: to retrieve the in-port connected to out.

   Stereotype: uses lock-evs as Template Method.

   Errors: out-port-busy
           port-not-connected

   Description: specialize lock-evs to provide a lock / unlock policy
                for the out-port."
  (if (lock out)
      (restart-case (error 'out-port-busy)
	(wait ()
	  (setf (waiting-queue out)
		(append (waiting-queue out)
			(list sim)))
	  (values nil nil))
	(cancel ()
	  (values nil nil)))
      (if (slot-boundp out 'peer-port)
	  (values (the in-port (peer-port out))
		  (lock-evs sim out obj))
	  (restart-case (error 'port-not-connected)
	    (cancel ()
	      (values nil nil))))))


(defmethod access-port ((sim simulator) (in in-port) (obj object))
  "Contract: simulator in-port object -> (or simulator nil)
                                         events

   Purpose: to retrieve the owner of in-port.

   Stereotype: uses lock-evs as Template Method.

   Errors: in-port-busy

   Description: specialize lock-evs to provide a lock / unlock policy
                for the in-port."
  (if (lock in)
      (restart-case (error 'in-port-busy)
	(wait ()
	  (setf (waiting-queue in)
		(append (waiting-queue in)
			(list sim)))
	  (values nil nil))
	(cancel ()
	  (values nil nil)))
      (values (the simulator (owner in))
	      (the list (lock-evs sim in obj)))))


(defmethod output-evs ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> events

   Purpose: to create a new event that will call input-evs on the
            destination of out and the given object.

   Description: specialize this method and wrap call-next-method with
                handler-bind."
  (multiple-value-bind (in out-events)
      (access-port sim out obj)
    (when in
      (multiple-value-bind (dest in-events)
	  (access-port sim in obj)
	(cons (make-instance 'event
			     :owner dest
			     :time (clock sim)
			     :fn #'input-evs
			     :args (list in obj))
	      (nconc out-events
		     in-events
		     (post-output-evs (remove-child sim obj)
				      out obj)))))))


(defmethod post-output-evs ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> events

   Purpose: post output-evs hook."
  (error "Specialize me!"))


(defmethod add-child ((sim simulator) (obj object))
  "Contract: simulator object -> simulator

   Purpose: to add obj into the children map of sim and to set sim as
            parent of obj."
  (setf (gethash (id obj) (children-by-id sim))
	obj)
  (setf (parent obj) sim)
  sim)


(defmethod remove-child ((sim simulator) (obj object))
  "Contract: simulator object -> simulator

   Purpose: to remove obj from the children map of sim."
  (remhash (id obj) (children-by-id sim))
  sim)


(defmethod add-children ((sim simulator) (ch list))
  "Contract: simulator objects -> simulator

   Purpose: to add recursively all the objects to sim."
  (if (null ch)
      sim
      (add-children (add-child sim (first ch))
		    (rest ch))))


(defmethod remove-children ((sim simulator) (ch list))
  "Contract: simulator objects -> simulator

   Purpose: to remove recursively all the objects from sim."
  (if (null ch)
      sim
      (remove-children (remove-child sim (first ch))
		       (rest ch))))


(defmethod schedule ((sce scenario) (events list))
  "Contract: scenario events -> scenario

   Purpose: to add the given events to the scenario, keeping them
            ordered by time."
  (flet ((sort-events (evs)
	   (stable-sort evs #'< :key #'time)))
    (setf (events sce)
	  (sort-events (append (events sce)
			       events)))
    sce))


(defmethod cancel-event ((ev event))
  "Contract: event -> nil

   Purpose: to set the canceled flag of ev by side effect."
  (setf (canceled-p ev) t)
  nil)


(defmethod synchronize ((sim simulator) tm)
  "Contract: simulator time -> simulator

   Purpose: to synchronize sim's internal clock with the given time."
  (declare (time-type tm))
  (setf (clock sim)
	tm)
  sim)


(defmethod evolve ((sim simulator) (ev event))
  "Contract: simulator event -> events

   Purpose: to apply the event to sim

   Example: (evolve bell hit-event) -> ring-event vibrate-event"
  (let ((res (apply (fn ev)
		    (append (list (synchronize sim (time ev)))
			    (when (slot-boundp ev 'args)
			      (args ev))))))
    (if (listp res)
	res
	(list res))))


(defmethod evolving ((sce scenario))
  "Contract: scenario -> simulator event

   Purpose: to return the next simulator that must execute its event.

   Example: (multiple-value-call #'evolve (evolving example-scenario))"
  (let ((ev (pop (events sce))))
    (cond ((null ev) (values nil nil))
	  ((canceled-p ev) (evolving sce))
	  (t (values (owner ev) ev)))))
