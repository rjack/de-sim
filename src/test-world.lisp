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


(defparameter *table*
  (make-instance 'table :id 0
		 :chairs (list (make-instance 'chair)
			       (make-instance 'chair))
		 :persons (list (make-instance 'person
					       :name "Geppo")
				(make-instance 'person
					       :name "Claudia")
				(make-instance 'person
					       :name "Peppo")
				(make-instance 'person
					       :name "Verruca"))
		 :events ()))
