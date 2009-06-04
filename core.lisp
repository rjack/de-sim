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

(defgeneric next-to-evolve (obj &optional next time)
  (:documentation "TODO"))


(defgeneric evolve (obj)
  (:documentation "obj can execute its event and evolve
  accordingly. Returns the new version of obj."))


(defgeneric components-list (sim)
  (:documentation "Returns the list representation of their
  components."))


(defgeneric imminent-event (obj &optional max-time)
  (:documentation "Return the function and the time of the next event
  of obj."))


(defgeneric schedule (actor function &key at relative-p)
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
    :initarg :id
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
    :type function)))


(defclass object (with-id)
  nil)


(defclass actor (object)
  ((in-ports
    :initarg :in-ports
    :initform (list)
    :accessor in-ports-of
    :type list)
   (out-ports
    :initarg :out-ports
    :initform (list)
    :accessor out-ports-of
    :type list)
   (events
    :accessor events-of
    :initform (list)
    :type list)))


(defclass simulator (actor)
  ((components
    :initarg :components
    :initform (list)
    :accessor components-of
    :type list)))


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



;; PARAMETERS

(defparameter *fresh-id* 0)

(defparameter *clock* 0)

(defparameter *out->in* (make-hash-table))



;; FUNCTIONS AND METHODS

(defun gettime ()
  *clock*)


(defun fresh-id ()
  (incf *fresh-id*))


(defmethod i/o-connect ((out out-port) (in in-port))
  (setf (gethash (id-of out) *out->in*)
	in)
  in)


(defmethod i/o-connected ((out out-port))
  (gethash (id-of out) *out->in*))


(defmethod i/o-disconnect ((out out-port))
  (remhash (id-of out) *out->in*))


(defmethod components-list ((sim simulator))
  (components-of sim))


(defmethod imminent-event ((obj object) &optional (max-time nil))
  (declare (ignore max-time))
  nil)


(defmethod imminent-event ((ac actor) &optional (max-time nil))
  (let ((ev (first (events-of ac))))
    (if (and (not (null ev))
	     (or (null max-time)
		 (<= (time-of ev) max-time)))
	ev
	(call-next-method))))


(defmethod imminent-event ((sim simulator) &optional (max-time nil))
  (first (sort (remove-if #'null
			  (mapcar (lambda (comp)
				    (imminent-event comp max-time))
				  (components-list sim)))
	       #'< :key #'time-of)))


(defmethod schedule ((act actor) (fn function)
		     &key (at 0) (relative-p t) (id 0 id-p))
  (declare (time-type at)
	   (id-type id))

  (let ((abs-time (+ at (if relative-p
			    (gettime)
			    0))))
    (if (< abs-time (gettime))
	(error 'error-invalid)
	(setf (events-of act)
	      (sort (cons (make-instance 'event
					 :id (if id-p
						 id
						 (fresh-id))
					 :time abs-time :fn fn)
			  (events-of act))
		    #'< :key #'time-of))))
  act)


(defmethod evolve ((obj object))
  (funcall (fn-of (imminent-event obj)) obj))
