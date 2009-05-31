(declaim (optimize debug safety (speed 0)))


(defpackage :pseudo-desim
  (:use :cl))


(deftype id-type ()
  '(integer 0))


(deftype time-type ()
  '(integer 0))


(deftype exec-time-type ()
  `(or time-type
       (member :never)))


(defclass with-id ()
  ((id :initarg :id :reader id :type id-type)))


(defclass with-name ()
  ((name :initarg :name :reader name :type string)))


(defclass object (with-id)
  nil)


(defclass input (with-id)
  nil)


(defclass output (with-id)
  nil)


(defclass audio-output (output)
  nil)


(defclass audio-input (input)
  nil)


(defclass with-audio-output ()
  ((audio-out :reader audio-out :type audio-output)))


(defclass with-audio-input ()
  ((audio-in :reader audio-in :type audio-input)))


(defclass person (object with-audio-output with-audio-input with-name)
  nil)




(defparameter *out->in* (make-hash-table))
(defparameter *in->obj* (make-hash-table))
(defparameter *fresh-id* 0)




(defun fresh-id ()
  (incf *fresh-id*))


(defmethod initialize-instance :after ((prs person) &key)
  (let ((ai (make-instance 'audio-input :id (fresh-id))))
    (setf (slot-value prs 'audio-in)
	  ai)
    (setf (gethash (id ai) *in->obj*)
	  prs))
  (setf (slot-value prs 'audio-out)
	(make-instance 'audio-output :id (fresh-id))))


(defclass event ()
  ((owner :initarg :owner :reader owner :type id-type)
   (exec-time :initarg :exec-time :reader exec-time)
   (fn :initarg :fn :reader fn)
   (args :initarg :args :reader args)))


(defmethod i/o-connect ((out output) (in input))
  (setf (gethash (id out) *out->in*)
	in))


(defmethod hear ((prs person) (msg string))
  nil)


(defmethod in ((ai audio-input) (prs person) something)
  (hear prs something))


(defmethod put ((in input) something)
  (multiple-value-bind (obj present-p)
      (gethash (id in) *in->obj*)
    (if present-p
	(in in obj something)
	(error "no object with that input!"))))


(defmethod put ((out output) something)
  (multiple-value-bind (in present-p)
      (gethash (id out) *out->in*)
    (if present-p
	(put in something)
	(error "output not connected to any input!"))))


(defmethod i/o-connect ((p1 person) (p2 person))
  (i/o-connect (audio-out p1) (audio-in p2)))



(defmethod tell ((prs person) (msg string))
  (put (audio-out prs) msg))


(defun run (ev objs)
  (apply (fn ev) (find-if (applicable ev) objs)))


(defun evolve (et evs objs conns)
  (let ((ev (imminent-event et evs)))
    (if (null ev)
	(error "done")
	(multiple-value-bind (new-et new-evs new-objs new-conns) (run ev objs)
	  (values new-et
		  (merge-events evs new-evs)
		  (merge-objects objs new-objs)
		  (merge-connnectors conns new-conns))))))



(defun ok-lets-try ()
  (let ((alice (make-instance 'person :name "Alice" :id (fresh-id)))
	(bob (make-instance 'person :name "Bob" :id (fresh-id))))
    (i/o-connect alice bob)
    (tell alice "ciao")))
