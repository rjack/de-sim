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

(defgeneric evolve (object events)
  (:documentation "obj can execute its event and evolve
  accordingly. Returns the new version of obj."))


(defgeneric components-list (simulator)
  (:documentation "Returns the list representation of their
  components."))


(defgeneric update-component (simulator object object-id)
  (:documentation "TODO"))


(defgeneric assign-path (object simulator)
  (:documentation "TODO"))


(defgeneric add-event (actor event)
  (:documentation "TODO"))


(defgeneric pop-event (actor)
  (:documentation "TODO"))


(defgeneric path (object)
  (:documentation "TODO"))


(defgeneric belongs (object event)
  (:documentation "TODO"))


(defgeneric i/o-connect (src dest)
  (:documentation "Connect src to dest."))


(defgeneric i/o-connected (src)
  (:documentation "Return values: connected object and connected-p
  boolean."))


(defgeneric i/o-disconnect (src)
  (:documentation "Disconnect src."))


(defgeneric handle-input (actor events in-port something)
  (:documentation "TODO"))


(defgeneric put (actor events out-port something)
  (:documentation "TODO"))


(defgeneric schedule (actor events event)
  (:documentation "TODO"))



;; TYPES

(deftype id-type ()
  '(integer 0))


(deftype time-type ()
  '(integer 0))



;; CLASSES

(defclass with-id ()     ; abstract
  ((id
    :reader id-of
    :type id-type
    :documentation "Unique id")))


(defclass event (with-id)
  ((owner-path
    :initarg :owner-path
    :initform (error ":owner-path missing")
    :accessor owner-path-of
    :type list)
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


(defclass object (with-id)
  ((parents-path
    :initarg :parents-path
    :initform (list)
    :accessor parents-path-of
    :type list
    :documentation "A list of id telling where the object is in the object tree.
    Example: given the tree
      sim-0
        sim-1
          act-2
          obj-3
        obj-4
        sim-5
          obj-6
    the path of obj-6 is (0 5)
    the paths of act-1 and obj-3 are the same (0 1)")))


(defclass actor (object)
  ((events
    :accessor events-of
    :initform (list)
    :type list)))


(defclass simulator (actor)
  ((components
    :initform (make-hash-table)
    :reader components-of
    :type hash-table)))


(defclass port (with-id)
  nil)


(defclass in-port (port)
  ((owner
    :initarg :owner
    :initform (error ":owner missing")
    :accessor owner-of
    :type actor)
   (busy
    :initarg :busy
    :initform nil
    :accessor busy
    :type boolean)))


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



;; PARAMETERS

(defparameter *fresh-object-id* 0)
(defparameter *fresh-event-id* 0)
(defparameter *fresh-port-id* 0)

(defparameter *clock* 0)

(defparameter *out->in* (make-hash-table))


;; INITIALIZE-INSTANCE

(defmethod initialize-instance :after ((pt port) &key)
  (setf (slot-value pt 'id)
	(incf *fresh-port-id*)))


(defmethod initialize-instance :after ((ev event) &key)
  (setf (slot-value ev 'id)
	(incf *fresh-event-id*)))


(defmethod initialize-instance :after ((obj object) &key)
  (setf (slot-value obj 'id)
	(incf *fresh-object-id*)))


;; PRINT-OBJECT

(defmethod print-object ((e event) s)
  (print-unreadable-object (e s :type t :identity t)
    (format s "owner-path: ~a time: ~a fn: ~a"
	    (owner-path-of e) (time-of e) (fn-of e))
    (when (slot-boundp e 'args)
      (format s " args: ~a" (args-of e)))))


;; FUNCTIONS AND METHODS


(let ((clock 0))
  (defun gettime (evs)
    (if (null evs)
	clock
	(setf clock
	      (time-of (first evs))))))


(defun path-starts-with (seq test)
  (equal (subseq seq 0 (length test))
	 test))


(defun sort-events (evs)
  (stable-sort evs #'< :key #'time-of))


(defmethod i/o-connect ((out out-port) (in in-port))
  (setf (gethash (id-of out) *out->in*)
	in)
  in)


(defmethod i/o-connected ((out out-port))
  (gethash (id-of out) *out->in*))


(defmethod i/o-disconnect ((out out-port))
  (remhash (id-of out) *out->in*))


;; schedulable
(defmethod handle-input ((act actor) (evs list) (in in-port)
			 (obj object))
  (error "specialize me!"))


;; schedulable
(defmethod handle-input ((act actor) (evs list) (in stream-in-port)
			 (obj null))
  (the (values actor list)
    (handle-input act evs in
		  (read-line (stream-of in) nil))))


;; schedulable
(defmethod put ((act actor) (evs list) (out stream-out-port)
		(str string))
  (format (stream-of out) "~&~a~%" str)
  (values act evs))


;; schedulable
(defmethod put ((act actor) (evs list) (out out-port) (obj object))
  (multiple-value-bind (in connected-p)
      (i/o-connected out)
    (with-accessors ((dest owner-of)) in
      (if (not connected-p)
	  (error 'error-invalid)
	  (the (values actor list)
	    (schedule dest evs
		      (make-instance 'event
				     :owner-path (path dest)
				     :time (gettime evs)
				     :fn #'handle-input
				     :args (list in obj))))))))


(defmethod update-component ((sim simulator) (obj object) id)
  (declare (id-type id))
  (assert (eql id (id-of obj)) nil "ids don't match!")
  (setf (gethash id (components-of sim))
	(assign-path obj sim))
  sim)


(defmethod update-component ((sim simulator) (obj null) id)
  (declare (id-type id))
  (multiple-value-bind (val val-p) (gethash id (components-of sim))
    (declare (ignore val))
    (assert val-p nil "cannot remove obj, not a component of sim!")
    (remhash id (components-of sim))
    sim))


(defmethod components-list ((sim simulator))
  (loop
     :for obj
     :being :the :hash-values :in (components-of sim)
     :collect obj))


(defmethod assign-path ((obj object) (sim simulator))
  (setf (parents-path-of obj)
	(append (parents-path-of sim)
		(list (id-of sim))))
  obj)


(defmethod path ((obj object))
  (append (parents-path-of obj)
	  (list (id-of obj))))


(defmethod belongs ((obj object) (ev event))
  (equal (path obj)
	 (owner-path-of ev)))


(defmethod schedule ((act actor) (evs list) (ev event))
  (values (the actor (add-event act ev))
	  (the list (sort-events (cons ev evs)))))


(defmethod add-event ((act actor) (ev event))
  (setf (events-of act)
	(sort-events (cons ev (events-of act))))
  act)


(defmethod pop-event ((act actor))
  (values act
	  (the event
	    (pop (events-of act)))))


(defmethod evolve ((act actor) (evs list))
  (multiple-value-bind (act ev)
      (pop-event act)
    (assert (eql ev (first evs)) nil "ev is not the first event!")
    (the (values actor list)
      (apply (fn-of ev)
	     (append (list act (rest evs))
		     (if (slot-boundp ev 'args)
			 (args-of ev)))))))


(defmethod evolve ((sim simulator) (evs list))
  (let ((next-ev (first evs)))
    (if (belongs sim next-ev)
	(call-next-method)
	(let ((evolving (find-if (lambda (component)
				   (path-starts-with (owner-path-of next-ev)
						     (path component)))
				 (components-list sim))))
	  (if (null evolving)
	      ;; next-ev's owner not found, discard it and keep going
	      (evolve sim (rest evs))
	      (multiple-value-bind (act evs) (evolve evolving evs)
		(values (the simulator
			  (update-component sim act (id-of act)))
			evs)))))))
