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


(in-package :de-sim)



;; GENERICS

;;TODO


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
    :type hash-table)
   (out-in-map
    :initform (make-hash-table)
    :reader out-in-map-of
    :type hash-table)))


(defclass scenario (simulator)
  ((events
    :accessor events-of
    :initform (list)
    :type list)))


(defclass event ()
  ((id
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
    :initform (error ":action missing")
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
  ((owner
    :initarg :owner
    :initform (error ":owner missing")
    :accessor owner-of
    :type simulator)))


(defclass out-port (port)
  nil)


(defclass stream-in-port (in-port)
  ((stream
    :initarg :input-stream
    :initform *query-io*
    :reader stream-of
    :type stream)))


(defclass stream-out-port (out-port)
  ((stream
    :initarg :output-stream
    :initform *query-io*
    :reader stream-of
    :type stream)))


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


(defmethod handle-input ((sim simulator) (in in-port) (obj object))
  "Contract: simulator in-port object -> events

   Purpose: to add object to the children of sim.

   Note: specializing methods must call-next-method."
  (add-child sim obj))


(defmethod connect-port ((sim simulator) (out out-port) (in in-port))
  "Contract: simulator out-port in-port -> simulator

   Purpose: to map the out-port to the in-port in the i/o-map of sim.

   Note: sim must be the parent of the owners of out-port and in-port."
  (setf (gethash (id-of out) (out-in-map-of sim))
	in)
  sim)


(defmethod disconnect-port ((sim simulator) (out out-port))
  "Contract: simulator out-port -> simulator

   Purpose: to remove the out-in mapping from the sim i/o map.

   Note: sim must be the parent of the owners of out-port and in-port."
  (remhash (id-of out) (out-in-map-of sim))
  sim)


(defmethod lock-port ((p port) (obj object))
  "Contract: port object -> (or event nil)

   Purpose: to lock the port and decide when unlock it.

   Stereotype: Template Method for access-port.

   Description: the default lock policy is to not lock at all.
                Specialize this method to provide your own lock
                policy. A timed lock must return the event that will
                unlock the port."
  (assert (not (lock-of p)) nil "Locking a locked port.")
  ;; do nothing
  nil)


(defmethod unlock-port ((in in-port))
  "TODO"
  nil)


(defmethod unlock-port ((out out-port))
  "TODO"
  nil)


(defmethod access-port ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> (or in-port nil)
                                          (or event nil)

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
			sim))
	  (values nil nil))
	(cancel ()
	  (values nil nil)))
      (multiple-value-bind (in in-p)
	  (gethash (id-of out)
		   (out-in-map-of (parent-of sim)))
	(if (not in-p)
	    (restart-case (error 'port-not-connected)
	      (cancel ()
		(values nil nil)))
	    (let ((unlock-event (lock-port out obj)))
	      (values in unlock-event))))))


(defmethod access-port ((sim simulator) (in in-port) (obj object))
  "Contract: simulator in-port object -> (or simulator nil)
                                         (or event nil)

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
			sim))
	  (values nil nil))
	(cancel ()
	  (values nil nil)))
      (values (the simulator (owner-of in))
	      (the event (lock-port in obj)))))


(defmethod output ((sim simulator) (out out-port) (obj object))
  "Contract: simulator out-port object -> events

   Purpose: to create a new event that will call handle-input on the
            destination of out and the given object.

   Description: specialize this method and wrap call-next-method with
                handler-bind."
  (multiple-value-bind (in out-unlock-event)
      (access-port sim out)
    (when in
      (multiple-value-bind (dest in-unlock-event)
	  (access-port sim in)
	(remove-child sim obj)
	(list out-unlock-event in-unlock-event
	      (make-instance 'event
			     :owner dest
			     :time (clock-of sim)
			     :fn #'handle-input
			     :args (list in obj)))))))


(defmethod add-child ((sim simulator) (obj object))
  "Contract: simulator object -> simulator

   Purpose: to add obj into the children map of sim."
  (setf (gethash (id-of obj) (children-by-id-of sim))
	obj)
  sim)


(defmethod remove-child ((sim simulator) (obj object))
  "Contract: simulator object -> simulator

   Purpose: to remove obj from the children map of sim."
  (remhash (id-of obj) (children-by-id-of sim))
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


(defmethod schedule ((sce scenario) &rest events)
  "Contract: scenario events -> scenario

   Purpose: to add the given events to the scenario, keeping them
            ordered by time."
  (flet ((sort-events (evs)
	   (stable-sort evs #'< :key #'time-of)))
    (setf (events-of sce)
	  (sort-events (append (events-of sce)
			       events)))
    sce))


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
  (apply (fn-of ev)
	 (append (list (synchronize sim (time-of ev)))
		 (when (slot-boundp ev 'args)
		   (args-of ev)))))


(defmethod evolving ((sce scenario))
  "Contract: scenario -> simulator event

   Purpose: to return the next simulator that must execute its event.

   Example: (multiple-value-call #'evolve (evolving example-scenario))"
  (let ((ev (pop (events-of sce))))
    (if (null ev)
	(values nil nil)
	(values (owner-of ev) ev))))
