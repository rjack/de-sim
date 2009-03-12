;; Code in the public domain.
;; No warranty.


(declaim (optimize safety debug (speed 0)))


(defpackage :org.altervista.rjack.test-world
  (:nicknames :tw)
  (:use :common-lisp :ds))

(in-package :tw)


(defclass table (world)
  ((chairs
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


(defclass person (simulated)
  ((description
    :initform "a person")

   (name
    :initarg :name
    :initform (error "missing :name")
    :reader name-of
    :type string
    :documentation "Name of this person.")))


(defclass chair (simulated)
  ((description
    :initform "a chair")
   
   (occupier
    :accessor occupier-of
    :type person
    :documentation "Reference to the person sitting on this chair, or
    nil if the chair is free.")))


(defparameter *table*
  (make-instance 'table :id 0
		 :chairs (list (make-instance 'chair :id 0)
			       (make-instance 'chair :id 1)
			       (make-instance 'chair :id 2)
			       (make-instance 'chair :id 3))
		 :persons (list (make-instance 'person
					       :id 0 :name "Geppo")
				(make-instance 'person
					       :id 1 :name "Claudia")
				(make-instance 'person
					       :id 2 :name "Peppo")
				(make-instance 'person
					       :id 3 :name "Verruca"))))
