(defpackage :pseudo-desim
  (:use :cl))


(defclass identifiable ()
  ((id :initarg :id)
   (version :initarg :version)))


(defclass object (identifiable)
  nil)


(defclass connector (identifiable)
  ((from :initarg :from :reader from)
   (to :initarg :from :reader to)
   (bandwidth :initarg :bandwidth :reader bandwidth)))


(defmethod -> ((a object) (b object))
  (make-instance 'connector
		 :id (fresh-id) :version 0
		 :from (id a) :to (id b)
		 :bandwidth :infinite)))


;; ogni object potrebbe estendere event anche solo per aver un modo
;; per riconoscere il tipo dei propri eventi
(defclass event ()
  ((owner :initarg :owner :reader owner)
   (exec-time :initarg :exec-time :reader exec-time)
   (fn :initarg :fn :reader fn)
   (args :initarg :args :reader args)))


(defun run (ev objs)
  (apply (fn ev) (find-if (applicable ev) objs)))


(defun evolve (et evs objs)
  (let ((ev (imminent-event et evs)))
    (if (null ev)
	(error "done")
	(multiple-value-bind (new-et new-evs new-objs) (run ev objs)
	  (evolve new-et (merge-evs evs new-evs)
		  (merge-objs objs new-objs))))))


