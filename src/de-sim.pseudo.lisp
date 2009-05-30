(declaim (optimize debug safety (speed 0)))


(defpackage :pseudo-desim
  (:use :cl))


(deftype id-type ()
  '(integer 0))


(deftype revision-type ()
  '(integer 0))


(deftype time-type ()
  '(integer 0))


(deftype exec-time-type ()
  `(or time-type
       (member :never)))


(defclass identifiable ()
  ((id :initarg :id :type id-type)
   (version :initarg :version :type version-type)))



(defclass object (identifiable)
  ((inputs)
   (outputs)))


(defclass event ()
  ((owner :initarg :owner :reader owner :type id-type)
   (exec-time :initarg :exec-time :reader exec-time)
   (fn :initarg :fn :reader fn)
   (args :initarg :args :reader args)))


(defclass connector (identifiable)
  ((from :initarg :from :reader from)
   (to :initarg :from :reader to)
   (bandwidth :initarg :bandwidth :reader bandwidth)))


(defmethod -> ((a object) (b object))
  (make-instance 'connector
		 :id (fresh-id) :version 0
		 :from (id a) :to (id b)
		 :bandwidth :infinite)))


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
