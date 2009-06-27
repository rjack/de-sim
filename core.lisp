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
  (:export :id-type :time-type
	   :object :id :id-of :parent :parent-of
	   :simulator :clock :clock-of
	   :scenario
	   :event :owner-of :time-of :fn-of :args-of
	   :port :lock-of :waiting-queue-of
	   :in-port :out-port
	   :i/o-transition-error
	   :port-not-connected
	   :out-port-busy
	   :in-port-busy
	   :wait :cancel
	   :obj=
	   :handle-input
	   :connect-port :disconnect-port
	   :lock-port :unlock-port
	   :in-port-ready :out-port-ready :access-port
	   :output :post-output
	   :leaving
	   :add-child :remove-child :add-children :remove-children
	   :schedule
	   :cancel-event
	   :evolving
	   :evolve))
(in-package :de-sim)



;; GENERICS

(defgeneric obj= (object object))
(defgeneric handle-input (simulator in-port object))
(defgeneric connect-port (out-port in-port))
(defgeneric disconnect-port (port))
(defgeneric lock-port (simulator port object))
(defgeneric lock-port (simulator port object))
(defgeneric unlock-port (simulator port))
(defgeneric unlock-port (simulator in-port))
(defgeneric unlock-port (simulator out-port))
(defgeneric in-port-ready (simulator out-port))
(defgeneric out-port-ready (simulator out-port))
(defgeneric access-port (simulator out-port object))
(defgeneric access-port (simulator in-port object))
(defgeneric output (simulator out-port object))
(defgeneric post-output (simulator out-port object))
(defgeneric add-child (simulator object))
(defgeneric remove-child (simulator object))
(defgeneric leaving (simulator out-port object))
(defgeneric add-children (simulator children))
(defgeneric remove-children (simulator children))
(defgeneric schedule (scenario events))
(defgeneric cancel-event (event))
(defgeneric synchronize (simulator tm))
(defgeneric evolve (simulator event))
(defgeneric evolving (scenario))


;; TYPES

(deftype id-type ()
  '(integer 0))


(deftype time-type ()
  '(integer 0))



;; CLASSES


(defclass object ()
  ((id
    :reader id-of
    :type id-type)
   (parent
    :accessor parent-of)))


(defclass simulator (object)
  ((clock
    :accessor clock-of
    :type time-type)
   (children-by-id
    :initform (make-hash-table)
    :reader children-by-id-of
    :type hash-table)))


(defclass scenario (simulator)
  ((events
    :accessor events-of
    :initform (list)
    :type list)))


(defclass event ()
  ((canceled-p
    :initform nil
    :accessor canceled-p-of
    :type boolean)
   (id
    :reader id-of
    :type id-type)
   (owner
    :initarg :owner
    :initform (error ":owner missing")
    :accessor owner-of
    :type simulator)
   (time
    :initarg :time
    :initform (error ":time missing")
    :accessor time-of
    :type time-type)
   (fn
    :initarg :fn
    :initform (error ":fn missing")
    :accessor fn-of
    :type function)
   (args
    :initarg :args
    ;; unbound if unused
    :accessor args-of
    :type list)))


(defclass port ()
  ((id
    :reader id-of
    :type id-type)
   (peer-port
    :accessor peer-port-of)
   (owner
    :initarg :owner
    :initform (error ":owner missing")
    :accessor owner-of
    :type simulator)
   (lock
    :initarg :lock
    :initform nil
    :accessor lock-of
    :type boolean)
   (waiting-queue
    :initform (list)
    :accessor waiting-queue-of
    :type list
    :documentation "List of sims waiting to use this port")))


(defclass in-port (port)
  nil)


(defclass out-port (port)
  nil)


;; CONDITIONS

(define-condition i/o-transition-error (error)
  nil)


(define-condition port-not-connected (i/o-transition-error)
  nil)


(define-condition out-port-busy (i/o-transition-error)
  nil)


(define-condition in-port-busy (i/o-transition-error)
  nil)


;; RESTART FUNCTIONS

(defun wait (c)
  (cond ((typep c 'out-port-busy)
	 (invoke-restart 'wait-out))
	((typep c 'in-port-busy)
	 (invoke-restart 'wait-in))
	(t (error "Bad condition type!"))))


(defun cancel (c)
  (declare (ignore c))
  (invoke-restart 'cancel))


;; INITIALIZE-INSTANCE

(let ((fresh-port-id 0))
  (defmethod initialize-instance :after ((pt port) &key)
    (setf (slot-value pt 'id)
	  (incf fresh-port-id))))


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
	    (time-of ev) (fn-of ev))
    (when (slot-boundp ev 'args)
      (format s " args: ~a" (args-of ev)))
    (format s "owner: ~a" (owner-of ev))))



;; FUNCTIONS AND METHODS


(defmethod obj= ((a object) (b object))
  "Contract: object object -> boolean

   Purpose: to test for equality."
  (eql (id-of a)
       (id-of b)))


(defmethod handle-input ((sim simulator) (in in-port) (obj object))
  "Contract: simulator in-port object -> events

   Purpose: to add object to the children of sim.

   Note: specializing methods must call-next-method."
  (add-child sim obj))


(defmethod connect-port ((out out-port) (in in-port))
  "Contract: simulator out-port in-port -> nil"
  (setf (peer-port-of out) in)
  (setf (peer-port-of in) out)
  nil)


(defmethod disconnect-port ((p port))
  "Contract: out-port -> nil"
  (slot-unbound 'port (owner-of (peer-port-of p)) 'peer-port)
  (slot-unbound 'port p 'peer-port)
  nil)


(defmethod lock-port ((sim simulator) (p port) (obj object))
  "Contract: simulator port object -> events

   Purpose: to lock the port and decide when unlock it.

   Stereotype: Template Method for access-port.

   Description: the default lock policy is to not lock.
                Specialize this method to provide your own lock
                policy."
  (assert (not (lock-of p)) nil "Locking a locked port.")
  nil)


(defmethod unlock-port :before ((sim simulator) (p port))
  (assert (lock-of p) nil "Unlocking a not locked port!")
  (setf (lock-of p) nil))


(defmethod unlock-port ((sim simulator) (in in-port))
  "Contract: out-port -> events"
  (let ((waiting (pop (waiting-queue-of in))))
    (when waiting
      (list (make-instance 'event
			   :owner waiting
			   :time (clock-of sim)
			   :fn #'in-port-ready
			   :args (list (peer-port-of in)))))))


(defmethod unlock-port ((sim simulator) (out out-port))
  "Contract: out-port -> events"
  (list (make-instance 'event
		       :owner sim
		       :time (clock-of sim)
		       :fn #'out-port-ready
		       :args (list out))))


(defmethod in-port-ready ((sim simulator) (out out-port))
  "Contract: simulator port -> events

   Description: specialize this method to generate events when the
                port connected to out gets unlocked."
  nil)


(defmethod out-port-ready ((sim simulator) (out out-port))
  "Contract: simulator port -> events

   Description: specialize this method to generate events when out
                gets unlocked."
  nil)


(defmethod access-port ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> (or in-port nil)
                                          events

   Purpose: to retrieve the in-port connected to out.

   Stereotype: uses lock-port as Template Method.

   Errors: out-port-busy
           port-not-connected

   Description: specialize lock-port to provide a lock / unlock policy
                for the out-port."
  (if (lock-of out)
      (restart-case (error 'out-port-busy)
	(wait ()
	  (setf (waiting-queue-of out)
		(append (waiting-queue-of out)
			(list sim)))
	  (values nil nil))
	(cancel ()
	  (values nil nil)))
      (if (slot-boundp out 'peer-port)
	  (values (the in-port (peer-port-of out))
		  (lock-port sim out obj))
	  (restart-case (error 'port-not-connected)
	    (cancel ()
	      (values nil nil))))))


(defmethod access-port ((sim simulator) (in in-port) (obj object))
  "Contract: simulator in-port object -> (or simulator nil)
                                         events

   Purpose: to retrieve the owner of in-port.

   Stereotype: uses lock-port as Template Method.

   Errors: in-port-busy

   Description: specialize lock-port to provide a lock / unlock policy
                for the in-port."
  (if (lock-of in)
      (restart-case (error 'in-port-busy)
	(wait ()
	  (setf (waiting-queue-of in)
		(append (waiting-queue-of in)
			(list sim)))
	  (values nil nil))
	(cancel ()
	  (values nil nil)))
      (values (the simulator (owner-of in))
	      (the list (lock-port sim in obj)))))


(defmethod output ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> events

   Purpose: to create a new event that will call handle-input on the
            destination of out and the given object.

   Description: specialize this method and wrap call-next-method with
                handler-bind."
  (multiple-value-bind (in out-events)
      (access-port sim out obj)
    (when in
      (multiple-value-bind (dest in-events)
	  (access-port sim in obj)
	(leaving (remove-child sim obj)
		 out obj)
	(cons (make-instance 'event
			     :owner dest
			     :time (clock-of sim)
			     :fn #'handle-input
			     :args (list in obj))
	      (nconc out-events
		     in-events
		     (post-output sim out obj)))))))


(defmethod post-output ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> events

   Purpose: post output hook."
  (error "Specialize me!"))


(defmethod add-child ((sim simulator) (obj object))
  "Contract: simulator object -> simulator

   Purpose: to add obj into the children map of sim and to set sim as
            parent of obj."
  (setf (gethash (id-of obj) (children-by-id-of sim))
	obj)
  (setf (parent-of obj) sim)
  sim)


(defmethod remove-child ((sim simulator) (obj object))
  "Contract: simulator object -> simulator

   Purpose: to remove obj from the children map of sim."
  (remhash (id-of obj) (children-by-id-of sim))
  sim)


(defmethod leaving ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> simulator

   Purpose: to provide a hook to process an object leaving the
            simulator through out."
  (error "specialize me!"))


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
	   (stable-sort evs #'< :key #'time-of)))
    (setf (events-of sce)
	  (sort-events (append (events-of sce)
			       events)))
    sce))


(defmethod cancel-event ((ev event))
  "Contract: event -> nil

   Purpose: to set the canceled flag of ev by side effect."
  (setf (canceled-p-of ev) t)
  nil)


(defmethod synchronize ((sim simulator) tm)
  "Contract: simulator time -> simulator

   Purpose: to synchronize sim's internal clock with the given time."
  (declare (time-type tm))
  (setf (clock-of sim)
	tm)
  sim)


(defmethod evolve ((sim simulator) (ev event))
  "Contract: simulator event -> events

   Purpose: to apply the event to sim

   Example: (evolve bell hit-event) -> ring-event vibrate-event"
  (let ((res (apply (fn-of ev)
		    (append (list (synchronize sim (time-of ev)))
			    (when (slot-boundp ev 'args)
			      (args-of ev))))))
    (if (listp res)
	res
	(list res))))


(defmethod evolving ((sce scenario))
  "Contract: scenario -> simulator event

   Purpose: to return the next simulator that must execute its event.

   Example: (multiple-value-call #'evolve (evolving example-scenario))"
  (let ((ev (pop (events-of sce))))
    (cond ((null ev) (values nil nil))
	  ((canceled-p-of ev) (evolving sce))
	  (t (values (owner-of ev) ev)))))
