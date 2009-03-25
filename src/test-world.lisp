;; Code in the public domain.
;; No warranty.


(declaim (optimize safety debug (speed 0)))


(defpackage :org.altervista.rjack.test-world
  (:nicknames :tw)
  (:use :common-lisp :ds))

(in-package :tw)


(defgeneric die (table person)
  (:documentation "Person dies and is removed from table."))


(defgeneric hear (table person from msg)
  (:documentation "Person hears a msg from another person."))


(defgeneric shout (table person msg)
  (:documentation "Person shouts msg"))


(defparameter *genid-table* -1)

(defclass table (world)
  ((ds::id
    :initform (incf *genid-table*))

   (persons
    :initarg :persons
    :accessor persons-of
    :type hash-table
    :documentation "Hash table of persons, indexed by their ids")))


(defparameter *genid-person* -1)

(defclass person (simulated)
  ((ds::id
    :initform (incf *genid-person*))

   (ds::description
    :initform "a person")

   (name
    :initarg :name
    :accessor name-of
    :type string
    :documentation "Name of this person.")

   (life-points
    :initarg :life-points
    :accessor life-points-of
    :type fixnum
    :documentation "When 0, person dies.")))


(defparameter *genid-table-event* -1)

(defclass table-event (event)
  ((ds::id
    :initform (incf *genid-table-event*))))


(defmethod die ((tab table) (p person))
  (format t "~A is dead :(" (name-of p))
  (remhash (id-of p) (persons-of tab)))



(defmethod hear :around ((tab table) (p person) (from person) (msg string))
  (if (multiple-value-bind (val present?)
	  (gethash (id-of p) (persons-of tab))
	(declare (ignorable val))
	present?)
      (call-next-method)
      (format t "hear event canceled: ~A not found" (name-of p))))


(defmethod hear ((tab table) (p person) (from person) (msg string))
  (let ((damage-points (random 10)))
    (decf (life-points-of p) damage-points)
    (format t "~A hears ~A's \"~A\" and suffers ~d damage points"
	    (name-of p) (name-of from) msg damage-points))
  (if (<= (life-points-of p) 0)
      (schedule tab (make-instance 'table-event
				   :time (+ (clock-of tab))
				   :action (lambda (tab)
					     (die tab p))))
      (schedule tab (make-instance 'table-event
				   :time (+ (clock-of tab)
					    (random 5))
				   :action (lambda (tab)
					     (shout tab p "AAAAA!"))))))


(defmethod shout :around ((tab table) (p person) (msg string))
  (if (multiple-value-bind (val present?)
	  (gethash (id-of p) (persons-of tab))
	(declare (ignorable val))
	present?)
      (call-next-method)
      (format t "shout event canceled: ~A not found" (name-of p))))


(defmethod shout :after ((tab table) (p person) (msg string))
  (maphash (lambda (key value)
	     (declare (ignorable key))
	     (when (not (eq value p))
	       (schedule tab (make-instance 'table-event
					    :time (clock-of tab)
					    :action (lambda (tab)
						      (hear tab value p msg))))))
	   (persons-of tab)))


(defmethod shout ((tab table) (p person) (msg string))
  (format t "~A shouts \"~A\"" (name-of p) msg))


(defparameter *geppo* (make-instance 'person :name "Geppo" :life-points 10))
(defparameter *claudia* (make-instance 'person :name "Claudia" :life-points 10))
(defparameter *verruca* (make-instance 'person :name "Verruca" :life-points 10))

(defparameter *table*
  (make-instance 'table
		 :persons (make-hash-table)
		 :events ()))

(setf (gethash (id-of *geppo*)
	       (persons-of *table*))
      *geppo*)

(setf (gethash (id-of *claudia*)
	       (persons-of *table*))
      *claudia*)

(setf (gethash (id-of *verruca*)
	       (persons-of *table*))
      *verruca*)

(schedule *table*
	  (make-instance 'table-event :time 3
			 :action (lambda (tab)
				   (shout tab *geppo* "AAAAAAAAAARGH!"))))
