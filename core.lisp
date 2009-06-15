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


(defclass event ()
  ((id
    :reader id-of
    :type id-type)
   (owner-path
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


(defclass object ()
  ((id
    :reader id-of
    :type id-type)
   (parent
    :accessor parent-of)
   (parents-path
    :accessor parents-path-of
    :type list
    :documentation "A list of id telling where the object is in the object tree.
    Example: given the tree
      sim-0
        sim-1
          sim-2
          obj-3
        obj-4
        sim-5
          obj-6
    the path of obj-6 is (0 5)
    the paths of sim-2 and obj-3 are the same (0 1)")))


(defclass simulator (object)
  ((children-by-id
    :initform (make-hash-table)
    :reader children-by-id-of
    :type hash-table)
   (events
    :accessor events-of
    :initform (list)
    :type list)
   (out-in-map
    :initform (make-hash-table)
    :reader out-in-map-of
    :type hash-table)))


(defclass port ()
  ((id
    :reader id-of
    :type id-type)
   (busy-p
    :initarg :busy-p
    :initform nil
    :accessor busy-p-of
    :type boolean)))


(defclass in-port (port)
  ((owner
    :initarg :owner
    :initform (error ":owner missing")
    :accessor owner-of
    :type simulator)
   (waiting-queue
    :initform (list)
    :accessor waiting-queue-of
    :type list
    :documentation "List of ids of the actors waiting to use this
    port")))


(defclass out-port (port)
  ((waiting-p
    :initform nil
    :accessor waiting-p-of
    :type boolean
    :documentation "True if the owner of this port is waiting in order
    to access it.")))


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
  ((sim :initarg :sim :reader sim-of :type simulator)
   (evs :initarg :evs :reader evs-of :type list)
   (port :initarg :port :reader port-of :type port)
   (obj :initarg :obj :reader obj-of :type object)))


(define-condition port-not-connected (i/o-transition-error)
  nil)


(define-condition out-port-busy (i/o-transition-error)
  nil)


(define-condition in-port-busy (i/o-transition-error)
  nil)


;; RESTART FUNCTIONS

(defun wait (c)
  (with-accessors ((sim sim-of) (evs evs-of) (port port-of)
		   (obj obj-of)) c
    (cond ((typep c 'out-port-busy)
	   (invoke-restart 'wait-out sim evs port obj))
	  ((typep c 'in-port-busy)
	   (invoke-restart 'wait-in sim evs port obj))
	  (t (error "Bad condition type!")))))


(defun retry (c)
  (with-accessors ((sim sim-of) (evs evs-of) (port port-of)
		   (obj obj-of)) c
    (invoke-restart 'retry sim evs port obj)))


(defun cancel (c)
  (with-accessors ((sim sim-of) (evs evs-of) (port port-of)
		   (obj obj-of)) c
    (invoke-restart 'cancel sim evs port obj)))


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


(defmethod i/o-connect ((sim simulator) (out out-port) (in in-port))
  (setf (gethash (id-of out) (out-in-map-of sim))
	in)
  sim)


(defmethod i/o-connected ((sim simulator) (out out-port))
  (gethash (id-of out) (out-in-map-of sim)))


(defmethod i/o-disconnect ((sim simulator) (out out-port))
  (remhash (id-of out) (out-in-map-of sim))
  sim)


(defmethod retry-delay ((sim simulator) (evs list) (out out-port)
			(obj object))
  10)


;; schedulable
(defmethod handle-input ((sim simulator) (evs list) (in in-port)
			 (obj object))
  (values (list (add-child sim obj))
	  evs))


(defmethod do-output ((sim simulator) (evs list) (out out-port)
		      (obj object))
  (restart-case (transition sim evs out obj)
    ;; waiting for the destination's in-port to be free
    (wait-in (sim evs in obj)
      (setf (waiting-queue-of in)
	    (append (waiting-queue-of in)
		    (id-of sim)))
      (values (list sim) evs out obj))

    ;; waiting for the out port to be free
    (wait-out (sim evs out obj)
      (setf (waiting-p-of out) t)
      (values (list sim) evs out obj))

    ;; try again after a computed delay
    (retry (sim evs out obj)
      (schedule sim evs (make-instance 'event
				       :owner-path (path sim)
				       :fn #'do-output
				       :time (+ (gettime evs)
						(retry-delay sim evs
							     out obj))
				       :args (list out obj))))

    ;; cancel output attempt
    (cancel (sim evs out obj)
      (values (list sim) evs out obj))))


; example of specializing method
;(defmethod do-output ((sim specialized-simulator) (evs list)
;		      (out out-port) (obj object))
  ;; WARNING! if output attempt fails, obj must be stored inside of sim
;  (handler-bind ((port-not-connected #'cancel)
;		 (out-port-busy #'retry)
;		 (in-port-busy #'wait))
;    (call-next-method)))


(defmethod transition ((sim simulator) (evs list) (out out-port)
		       (obj object))
  (multiple-value-bind (in connected-p)
      (i/o-connected (parent-of sim) out)
    (with-accessors ((dest owner-of)) in
      (cond ((not connected-p)
	     (error 'port-not-connected
		    :sim sim :evs evs :port out :obj obj))
	    ((busy-p-of out)
	     (error 'out-port-busy
		    :sim sim :evs evs :port out :obj obj))
	    ((busy-p-of in)
	     (error 'in-port-busy
		    :sim sim :evs evs :port in :obj obj))
	    (t (schedule dest obj evs
			 (make-instance 'event
					:owner-path (path dest)
					:time (gettime evs)
					:fn #'handle-input
					:args (list in obj))))))))


(defmethod add-child ((sim simulator) (obj object))
  (setf (gethash (id-of obj) (children-by-id-of sim))
	(assign-path obj sim))
  sim)


(defmethod remove-child ((sim simulator) (obj object))
  (multiple-value-bind (val val-p) (gethash (id-of obj)
					    (children-by-id-of sim))
    (declare (ignore val))
    (assert val-p nil "cannot remove obj, not a child of sim!")
    (remhash (id-of obj) (children-by-id-of sim))
    sim))


(defmethod add-children ((sim simulator) (ch list))
  (if (null ch)
      sim
      (add-children (add-child sim (first ch))
		    (rest ch))))


(defmethod children ((sim simulator))
  (loop
     :for obj
     :being :the :hash-values :in (children-by-id-of sim)
     :collect obj))


(defmethod assign-path ((obj object) (sim simulator))
  (setf (parents-path-of obj)
	(path sim))
  obj)


(defmethod path ((obj object))
  (append (parents-path-of obj)
	  (list (id-of obj))))


(defmethod belongs ((obj object) (ev event))
  (equal (path obj)
	 (owner-path-of ev)))


(defmethod schedule ((sim simulator) (evs list) (ev event))
  (values (list (add-event sim ev))
	  (sort-events (cons ev evs))))


(defmethod add-event ((sim simulator) (ev event))
  (setf (events-of sim)
	(sort-events (cons ev (events-of sim))))
  sim)


(defmethod pop-event ((sim simulator))
  (values sim
	  (the event
	    (pop (events-of sim)))))


(defmethod evolve ((sim simulator) (evs list))
  (let ((next-ev (first evs)))
    (if (belongs sim next-ev)
	(multiple-value-bind (sim ev)
	    (pop-event sim)
	  (assert (eql ev (first evs)) nil "ev is not the first event!")
	  (apply (fn-of ev)
		 (append (list sim (rest evs))
			 (when (slot-boundp ev 'args)
			   (args-of ev)))))
	;; else next-ev belongs to one of the children
	(let ((evolving (find-if (lambda (child)
				   (path-starts-with (owner-path-of next-ev)
						     (path child)))
				 (children sim))))
	  (if (null evolving)
	      ;; next-ev's owner not found
	      (values (list sim) evs)
	      ;; else evolve and return values
	      (multiple-value-bind (new-children new-evs)
		  (evolve evolving evs)
		(values (update-children sim new-children)
			new-evs)))))))
