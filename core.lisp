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


(declaim (optimize debug safety (speed 0))
	 (sb-ext:muffle-conditions sb-ext:compiler-note))


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


(defgeneric assign-path (object simulator)
  (:documentation "TODO"))


(defgeneric add-event (actor event)
  (:documentation "TODO"))


(defgeneric path (object)
  (:documentation "TODO"))


(defgeneric belongs (object event)
  (:documentation "TODO"))


(defgeneric schedule (events delay function actor &rest args)
  (:documentation "TODO"))


(defgeneric i/o-connect (src dest)
  (:documentation "Connect src to dest."))


(defgeneric i/o-connected (src)
  (:documentation "Return values: connected object and connected-p
  boolean."))


(defgeneric i/o-disconnect (src)
  (:documentation "Disconnect src."))



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
    :initform (error ":parents-path missing")
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
  ;; components slots in subclasses
  nil)


(defclass port (with-id)
  ((owner
    :initarg :owner
    :initform (error ":owner missing")
    :accessor owner-of
    :type actor)))


(defclass in-port (port)
  nil)


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


;; FUNCTIONS AND METHODS

(defun path-starts-with (seq test)
  (equal (subseq seq 0 (length test))
	 test))


(defun sort-events (evs)
  (sort evs #'< :key #'time-of))


(defmethod i/o-connect ((out out-port) (in in-port))
  (setf (gethash (id-of out) *out->in*)
	in)
  in)


(defmethod i/o-connected ((out out-port))
  (gethash (id-of out) *out->in*))


(defmethod i/o-disconnect ((out out-port))
  (remhash (id-of out) *out->in*))


(defmethod components-list ((sim simulator))
  (error "specialize me!"))


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


(defmethod schedule ((all-events list) at (fn function) (act actor)
		     &rest args)
  (declare (time-type at))
  (let ((ev (apply 'make-instance
		   'event
		   ;; building arguments for make-instance:
		   ;; if args is nil, leave slot unbound
		   (append (list :time at
				 :fn fn
				 :owner-path (path act))
			   (if (null args)
			       nil
			       (list :args args))))))
    (values (the list (sort-events (cons ev all-events)))
	    (the actor (add-event act ev))
	    (the event ev))))


(defmethod add-event ((act actor) (ev event))
  (setf (events-of act)
	(sort-events (cons ev (events-of act)))))


(defmethod evolve ((act actor) (evs list))
  (let ((ev (first evs)))
    (if (belongs act ev)
	(apply (fn-of ev)
	       (append (list act (time-of ev))
		       (if (slot-boundp ev 'args)
			   (args-of ev))))
	(error 'error-invalid))))


(defmethod evolve ((sim simulator) (evs list))
  (let ((next-ev (first evs)))
    (if (belongs sim next-ev)
	(call-next-method)
	(let ((evolving (find-if (lambda (component)
				   (path-starts-with (owner-path-of next-ev)
						     (path component)))
				 (components-list sim))))
	  (if evolving
	      (evolve evolving evs)
	      ;; next-ev's owner not found, discard it and keep going
	      (evolve sim (rest evs)))))))
