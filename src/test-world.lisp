;; Code in the public domain.
;; No warranty.


(declaim (optimize safety debug (speed 0)))


(defpackage :org.altervista.rjack.test-world
  (:nicknames :tw)
  (:use :common-lisp :ds))

(in-package :tw)


(defparameter *genid-table* -1)

(defclass table (world)
  ((ds::id
    :initform (incf *genid-table*))

   (chairs
    :initarg :chairs
    :initform (error "missing :chairs")
    :accessor chairs-of
    :type list
    :documentation "List of chairs.")

   (persons
    :initarg :persons
    :initform (error "missing :persons")
    :accessor persons-of
    :type list
    :documentation "List of persons.")))


(defparameter *genid-person* -1)

(defclass person (simulated)
  ((ds::id
    :initform (incf *genid-person*))

   (ds::description
    :initform "a person")

   (name
    :initarg :name
    :initform (error "missing :name")
    :reader name-of
    :type string
    :documentation "Name of this person.")))


(defparameter *genid-chair* -1)

(defclass chair (simulated)
  ((ds::id
    :initform (incf *genid-chair*))

   (ds::description
    :initform "a chair")
   
   (occupier
    :accessor occupier-of
    :type person
    :documentation "Reference to the person sitting on this chair, or
    nil if the chair is free.")))


(defparameter *genid-table-event* -1)

(defclass table-event (event)
  ((ds::id
    :initform (incf *genid-table-event*))))


(defmethod !transform ((dest table) (source table))
  (setf (chairs-of dest) (chairs-of source))
  (setf (persons-of dest) (persons-of source))
  (call-next-method))


(defmethod clone ((tab table))
  (!transform (make-instance 'table
			     :events nil
			     :chairs nil
			     :persons nil)
	      tab))


(defparameter *geppo* (make-instance 'person :name "Geppo"))
(defparameter *claudia* (make-instance 'person :name "Claudia"))
(defparameter *verruca* (make-instance 'person :name "Verruca"))

(defparameter *table-events*
  (list (make-instance 'table-event :time 3
		       :causes (list *geppo*)
		       :!action (lambda (world)
				  (format t
"~&Geppo does something in world ~a." (id-of world))))))

(defparameter *table*
  (make-instance 'table
		 :persons (list *geppo* *claudia* *verruca*)
		 :chairs (list (make-instance 'chair))
		 :events *table-events*))