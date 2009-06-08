;; CSP-SIM
;; Can & String Phone SIMulator

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


;; OVERVIEW
;;
;; CSP-SIM simulates a simple scenario in which two cans are connected
;; by a string (let's call it wire from now). A person talks into a
;; can and the wire will transmit the sound to the other can, where
;; the second person will hear it.
;;
;; Components:
;;
;;     thought-in-port  --> +--------+ --> voice-out-port
;;                          | PERSON |
;;     thought-out-port <-- +--------+ <-- voice-in-port
;;
;;
;;     voice-in-port  --> +-----+ --> vibration-out-port
;;                        | CAN |
;;     voice-out-port <-- +-----+ <-- vibration-in-port
;;
;;
;;     vibration-in-port  --> +------+ --> vibration-out-port
;;                            | WIRE |
;;     vibration-out-port <-- +------+ <-- vibration-in-port


(defpackage :org.altervista.rjack.csp-sim
  (:nicknames :csp-sim)
  (:use :cl :de-sim))

(in-package :csp-sim)


(defgeneric think (person events)
  (:documentation "TODO"))


(defclass vibration-in-port (in-port)
  nil)


(defclass vibration-out-port (out-port)
  nil)


(defclass voice-in-port (in-port)
  nil)


(defclass voice-out-port (out-port)
  nil)


(defclass thought-in-port (stream-in-port)
  nil)


(defclass thought-out-port (stream-out-port)
  nil)


(defclass voice (object)
  ((message
    :initarg :message
    :initform (error ":message missing")
    :reader message-of
    :type string)))


(defclass vibration (object)
  ((message
    :initarg :message
    :initform (error ":message missing")
    :reader message-of
    :type string)))


(defclass person (actor)
  ((name
    :initarg :name
    :initform (error ":name missing")
    :reader name-of
    :type string)
   (thought-in
    :accessor thought-in-of
    :type thought-in-port)
   (thought-out
    :accessor thought-out-of
    :type thought-out-port)
   (voice-in
    :accessor voice-in-of
    :type voice-in-port)
   (voice-out
    :accessor voice-out-of
    :type voice-out-port)))


(defclass can (actor)
  ((voice-in
    :accessor voice-in-of
    :type voice-in-port)
   (voice-out
    :accessor voice-out-of
    :type voice-out-port)
   (vibration-in
    :accessor vibration-in-of
    :type vibration-in-port)
   (vibration-out
    :accessor vibration-out-of
    :type vibration-out-port)))


(defclass wire (actor)
  ((a-vibration-in
    :accessor a-vibration-in-of
    :type vibration-in-port)
   (a-vibration-out
    :accessor a-vibration-out-of
    :type vibration-out-port)
   (b-vibration-in
    :accessor b-vibration-in-of
    :type vibration-in-port)
   (b-vibration-out
    :accessor b-vibration-out-of
    :type vibration-out-port)))


(defclass csp-scenario (simulator)
  ((persons
    :accessor persons-of
    :type (cons person person))
   (cans
    :accessor cans-of
    :type (cons can can))
   (wire
    :accessor wire-of
    :type wire)))


;; INITIALIZE INSTANCE

(defmethod initialize-instance :after ((c can) &key)
  (with-accessors ((voi< voice-in-of) (voi> voice-out-of)
		   (vib< vibration-in-of) (vib> vibration-out-of)) c
    (setf voi< (make-instance 'voice-in-port :owner c))
    (setf voi> (make-instance 'voice-out-port :owner c))
    (setf vib< (make-instance 'vibration-in-port :owner c))
    (setf vib> (make-instance 'vibration-out-port :owner c))))


(defmethod initialize-instance :after ((w wire) &key)
  (with-accessors ((a< a-vibration-in-of) (a> a-vibration-out-of)
		   (b< b-vibration-in-of) (b> b-vibration-out-of)) w
    (setf a< (make-instance 'vibration-in-port :owner w))
    (setf a> (make-instance 'vibration-out-port :owner w))
    (setf b< (make-instance 'vibration-in-port :owner w))
    (setf b> (make-instance 'vibration-out-port :owner w))))


(defmethod initialize-instance :after ((p person) &key)
  (with-accessors ((tho< thought-in-of) (tho> thought-out-of)
		   (voi< voice-in-of) (voi> voice-out-of)) p
    (setf voi< (make-instance 'voice-in-port :owner p))
    (setf voi> (make-instance 'voice-out-port :owner p))
    (setf tho< (make-instance 'thought-in-port :owner p))
    (setf tho> (make-instance 'thought-out-port :owner p))))


(defun setup-csp ()
  (let* ((csp (make-instance 'csp-scenario))
	 (a (make-instance 'person :parents-path (path csp)
			   :name "Alice"))
	 (b (make-instance 'person :parents-path (path csp)
			   :name "Bob"))
	 (ac (make-instance 'can :parents-path (path csp)))
	 (bc (make-instance 'can :parents-path (path csp)))
	 (w (make-instance 'wire :parents-path (path csp))))
    ;; assign components
    (setf (cans-of csp)
	  (cons ac bc))
    (setf (wire-of csp)
	  w)
    (setf (persons-of csp)
	  (cons a b))
    ;; setup i/o connections between ports
    ;; Alice with her can
    (i/o-connect (voice-out-of a)
		 (voice-in-of ac))
    ;; Bob with his can
    (i/o-connect (voice-out-of b)
		 (voice-in-of bc))
    ;; Alice's can with Alice's end of wire
    (i/o-connect (vibration-out-of ac)
		 (a-vibration-in-of w))
    (i/o-connect (a-vibration-out-of w)
		 (vibration-in-of ac))
    ;; Bob's can with Bob's end of wire
    (i/o-connect (vibration-out-of bc)
		 (b-vibration-in-of w))
    (i/o-connect (b-vibration-out-of w)
		 (vibration-in-of bc))
    csp))


(defun vibration->voice (vi)
  (declare (vibration vi))
  (the vibration
    (make-instance 'vibration :message (message-of vi))))


(defun voice->vibration (vo)
  (declare (voice vo))
  (the vibration
    (make-instance 'vibration :message (message-of vo))))


(defmethod think ((p person) (evs list))
  (schedule p (make-instance 'i/o-event
			     :time (gettime evs)
			     :fn #'put
			     :args (list (thought-in-of p)))))


(defmethod handle-input ((p person) (in thought-in-port) (str string))
  (put (voice-out-of p)
       (make-instance 'voice :message str)))


(defmethod run ((csp csp-scenario))
  (labels ((run-rec (csp evs)
	     (if (null evs)
		 csp
		 (multiple-value-call #'run-rec (evolve csp evs)))))
    (multiple-value-bind (evs csp)
	(schedule (events-of csp) 0 #'think (car (persons-of csp)))
      (run-rec csp evs))))
