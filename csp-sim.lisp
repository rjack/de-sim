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
;(declaim (optimize (debug 0) (safety 0) speed))


;; OVERVIEW
;;
;; CSP-SIM simulates a simple scenario in which two cans are connected
;; by a string (let's call it wire from now). A person talks into a
;; can and the wire will transmit the sound to the other can, where
;; the second person will hear it.
;;
;; Components:
;;
;;     +--------+ --> voice-out-port
;;     | PERSON |
;;     +--------+ <-- voice-in-port
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



(defclass vibration-in-port (in-port)
  nil)


(defclass vibration-out-port (out-port)
  nil)


(defclass voice-in-port (in-port)
  nil)


(defclass voice-out-port (out-port)
  nil)


(defclass vibration (object)
  ((message
    :initarg :message
    :initform (error ":message missing")
    :reader message-of
    :type string)
   (duration
    :initarg :duration
    :initform (error ":duration missing")
    :reader duration-of
    :type time-type)))


(defclass voice (object)
  ((message
    :initarg :message
    :initform (error ":message missing")
    :reader message-of
    :type string)
   (duration
    :reader duration-of
    :type time-type)))


(defclass person (simulator)
  ((name
    :initarg :name
    :initform (error ":name missing")
    :reader name-of
    :type string)
   (voice-in
    :accessor voice-in-of
    :type voice-in-port)
   (voice-out
    :accessor voice-out-of
    :type voice-out-port)))


(defclass can (simulator)
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


(defclass wire (simulator)
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


(defclass csp (scenario)
  ((alice
    :initarg :alice
    :initform (error ":alice missing")
    :accessor alice-of
    :type person)
   (bob
    :initarg :bob
    :initform (error ":bob missing")
    :accessor bob-of
    :type person)
   (a-can
    :initarg :a-can
    :initform (error ":a-can missing")
    :accessor a-can-of
    :type can)
   (b-can
    :initarg :b-can
    :initform (error ":b-can missing")
    :accessor b-can-of
    :type can)
   (wire
    :initarg :wire
    :initform (error ":wire missing")
    :accessor wire-of
    :type wire)))


;; INITIALIZE INSTANCE


(defmethod initialize-instance :after ((v voice) &key)
  (setf (slot-value v 'duration)
	(length (message-of v))))


(defmethod initialize-instance :after ((c can) &key)
  (with-accessors ((voi< voice-in-of) (voi> voice-out-of)
		   (vib< vibration-in-of) (vib> vibration-out-of)) c
    (setf voi< (make-instance 'voice-in-port :owner c))
    (setf voi> (make-instance 'voice-out-port))
    (setf vib< (make-instance 'vibration-in-port :owner c))
    (setf vib> (make-instance 'vibration-out-port))))


(defmethod initialize-instance :after ((w wire) &key)
  (with-accessors ((a< a-vibration-in-of) (a> a-vibration-out-of)
		   (b< b-vibration-in-of) (b> b-vibration-out-of)) w
    (setf a< (make-instance 'vibration-in-port :owner w))
    (setf a> (make-instance 'vibration-out-port))
    (setf b< (make-instance 'vibration-in-port :owner w))
    (setf b> (make-instance 'vibration-out-port))))


(defmethod initialize-instance :after ((p person) &key)
  (with-accessors ((voi< voice-in-of) (voi> voice-out-of)) p
    (setf voi< (make-instance 'voice-in-port :owner p))
    (setf voi> (make-instance 'voice-out-port))))


(defmethod initialize-instance :after ((csp csp) &key)
  (with-accessors ((alice alice-of) (bob bob-of)
		   (a-can a-can-of) (b-can b-can-of)
		   (wire wire-of)) csp
    (add-children csp (list alice bob a-can b-can wire))
    ;; alice --voice--> a-can
    (connect-port csp (voice-out-of alice) (voice-in-of a-can))
    ;; a-can --voice--> alice
    (connect-port csp (voice-out-of a-can) (voice-in-of alice))
    ;; bob --voice--> b-can
    (connect-port csp (voice-out-of bob) (voice-in-of b-can))
    ;; b-can --voice--> bob
    (connect-port csp (voice-out-of b-can) (voice-in-of bob))
    ;; a-can --vibration--> wire
    (connect-port csp (vibration-out-of a-can) (a-vibration-in-of wire))
    ;; wire --vibration--> a-can
    (connect-port csp (a-vibration-out-of wire) (vibration-in-of a-can))
    ;; b-can --vibration--> wire
    (connect-port csp (vibration-out-of b-can) (b-vibration-in-of wire))
    ;; wire --vibration--> b-can
    (connect-port csp (b-vibration-out-of wire) (vibration-in-of b-can))))


(defun vibration->voice (vi)
  (declare (vibration vi))
  (make-instance 'voice :message (message-of vi)))


(defun voice->vibration (vo)
  (declare (voice vo))
  (make-instance 'vibration :message (message-of vo)
		 :duration (duration-of vo)))


(defmethod print-object ((p person) s)
  (print-unreadable-object (p s)
    (format s "~a" (name-of p))))


(defmethod print-object ((vo voice) s)
  (print-unreadable-object (vo s)
    (format s "~a (~a)" (message-of vo) (duration-of vo))))

;; VOICE LOCK POLICY

(defmethod lock-port ((p person) (voice-out voice-out-port)
		      (vo voice))
  (setf (lock-of voice-out) t)
  (list (make-instance 'event
		       :owner p
		       :time (+ (clock-of p)
				(duration-of vo))
		       :fn #'unlock-port
		       :args (list voice-out))))


;; PERSON BEHAVIOUR


(let ((phrases (list "Uh?" "Hello!" "Yes" "Really?" "No"
		     "Did you say something?"
		     "Please go away" "I'm calling the police"
		     "I love you!" "Do you love me?" "I hate you!"
		     "Just shut up.")))

  (defun random-phrase ()
    (make-instance 'voice
		   :message (nth (random (length phrases))
				 phrases))))


(defmethod start-talking ((p person))
  (make-instance 'event
		 :owner p
		 :time (clock-of p)
		 :fn #'output
		 :args (list (voice-out-of p)
			     (random-phrase))))


(defmethod port-ready ((p person) (voice-out voice-out-port))
  (list (make-instance 'event
		       :owner p
		       :time (clock-of p)
		       :fn #'start-talking)))


(defmethod handle-input ((p person) (in voice-in-port)
			 (vo voice))
  (call-next-method)
  (remove-child p vo)
  nil)


;; CAN BEHAVIOUR

(defmethod handle-input ((c can) (in voice-in-port)
			 (vo voice))
  "voice-in ---> vibration-out"
  (call-next-method)
  (make-instance 'event
		 :owner c
		 :time (clock-of c)
		 :fn #'output
		 :args (list (vibration-out-of c)
			     (voice->vibration vo))))


(defmethod handle-input ((c can) (in vibration-in-port)
			 (vi vibration))
  "vibration-in ---> voice-out"
  (call-next-method)
  (make-instance 'event
		 :owner c
		 :time (clock-of c)
		 :fn #'output
		 :args (list (voice-out-of c)
			     (vibration->voice vi))))


;; WIRE BEHAVIOUR

(defmethod handle-input ((w wire) (in vibration-in-port)
			 (vi vibration))
  "vibration-in ---> vibration-out"
  (call-next-method)
  (let ((out (if (eq in (a-vibration-in-of w))
		 (b-vibration-out-of w)
		 (a-vibration-out-of w))))
    (make-instance 'event
		   :owner w
		   :time (clock-of w)
		   :fn #'output
		   :args (list out vi))))


;; LOGGING

(defmethod handle-input :after ((sim simulator) (in in-port)
				(obj object))
  (format t "~&~a - ~a receives ~a through ~a~%"
	  (clock-of sim) sim obj in))


(defmethod output :after ((sim simulator) (out out-port)
			  (obj object))
  (format t "~&~a - ~a outputs ~a through ~a~%"
	  (clock-of sim) sim obj out))


(defun fresh-csp ()
  (make-instance 'csp
		 :alice (make-instance 'person :name "Alice")
		 :bob (make-instance 'person :name "Bob")
		 :a-can (make-instance 'can)
		 :b-can (make-instance 'can)
		 :wire (make-instance 'wire)))
