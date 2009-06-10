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


(defmethod initialize-instance :after ((csp csp-scenario) &key)
  (with-accessors ((alice alice-of) (bob bob-of)
		   (a-can a-can-of) (b-can b-can-of)
		   (wire wire-of)) csp
    (labels ((set-all (csp comps)
	       (if (null comps)
		   csp
		   (let ((fst (first comps)))
		     (set-all (update-component csp fst (id-of fst))
			      (rest comps))))))
      (set-all csp (list alice bob a-can b-can wire))
      ;; setup i/o connections between ports
      ;; alice --voice--> a-can
      (i/o-connect (voice-out-of alice)
		   (voice-in-of a-can))
      ;; a-can --voice--> alice
      (i/o-connect (voice-out-of a-can)
		   (voice-in-of alice))
      ;; bob --voice--> b-can
      (i/o-connect (voice-out-of bob)
		   (voice-in-of b-can))
      ;; b-can --voice--> bob
      (i/o-connect (voice-out-of b-can)
		   (voice-in-of bob))
      ;; a-can --vibration--> wire
      (i/o-connect (vibration-out-of a-can)
		   (a-vibration-in-of wire))
      ;; wire --vibration--> a-can
      (i/o-connect (a-vibration-out-of wire)
		   (vibration-in-of a-can))
      ;; b-can --vibration--> wire
      (i/o-connect (vibration-out-of b-can)
		   (b-vibration-in-of wire))
      ;; wire --vibration--> b-can
      (i/o-connect (b-vibration-out-of wire)
		   (vibration-in-of b-can)))))


(defun vibration->voice (vi)
  (declare (vibration vi))
  (make-instance 'voice :message (message-of vi)))


(defun voice->vibration (vo)
  (declare (voice vo))
  (make-instance 'vibration :message (message-of vo)))


(defmethod print-object ((p person) s)
  (print-unreadable-object (p s :type t :identity t)
    (format s "path: ~a name: ~a" (path p) (name-of p))))


(defmethod print-object ((csp csp-scenario) s)
  (with-accessors ((alice alice-of) (bob bob-of)
		   (a-can a-can-of) (b-can b-can-of)
		   (wire wire-of)) csp
    (print-unreadable-object (csp s :type t :identity t)
      (format s "path: ~a" (path csp)))))



;; PERSON BEHAVIOUR

;; schedulable
(defmethod think ((p person) (evs list))
  (schedule p evs (make-instance 'event
				 :owner-path (path p)
				 :time (gettime evs)
				 :fn #'handle-input
				 :args (list (thought-in-of p)
					     nil))))


;; schedulable
(defmethod handle-input ((p person) (evs list) (in thought-in-port)
			 (str string))
  "thought-in ---> voice-out"
  (let ((vo (make-instance 'voice :message str)))
    (schedule p evs (make-instance 'event
				   :owner-path (path p)
				   :time (gettime evs)
				   :fn #'put
				   :args (list (voice-out-of p)
					       vo)))))


;; schedulable
(defmethod handle-input ((p person) (evs list) (in voice-in-port)
			 (vo voice))
  "voice-in ---> thought-out"
  (schedule p evs (make-instance 'event
				 :owner-path (path p)
				 :time (gettime evs)
				 :fn #'put
				 :args (list (thought-out-of p)
					     (message-of vo)))))


;; CAN BEHAVIOUR

;; schedulable
(defmethod handle-input ((c can) (evs list) (in voice-in-port)
			 (vo voice))
  "voice-in ---> vibration-out"
  (schedule c evs (make-instance 'event
				 :owner-path (path c)
				 :time (gettime evs)
				 :fn #'put
				 :args (list (vibration-out-of c)
					     (voice->vibration vo)))))


(defmethod handle-input ((c can) (evs list) (in vibration-in-port)
			 (vi vibration))
  "vibration-in ---> voice-out"
  (schedule c evs (make-instance 'event
				 :owner-path (path c)
				 :time (gettime evs)
				 :fn #'put
				 :args (list (voice-out-of c)
					     (vibration->voice vi)))))


;; WIRE BEHAVIOUR

;; schedulable
(defmethod handle-input ((w wire) (evs list) (in vibration-in-port)
			 (vi vibration))
  "vibration-in ---> vibration-out"
  (let ((out (if (eq in (a-vibration-in-of w))
		 (b-vibration-out-of w)
		 (a-vibration-out-of w))))
    (schedule w evs (make-instance 'event
				   :owner-path (path w)
				   :time (gettime evs)
				   :fn #'put
				   :args (list out vi)))))

(defun fresh-csp-scenario ()
  (make-instance 'csp-scenario
		 :alice (make-instance 'person :name "Alice")
		 :bob (make-instance 'person :name "Bob")
		 :a-can (make-instance 'can)
		 :b-can (make-instance 'can)
		 :wire (make-instance 'wire)))


(defmethod run ((csp csp-scenario))
  (labels ((run-rec (csp evs)
	     (if (null evs)
		 csp
		 (multiple-value-call #'run-rec
		   (evolve csp evs)))))
    (with-accessors ((alice alice-of)) csp
      (multiple-value-bind (act evs)
	  (schedule alice nil
		    (make-instance 'event
				   :owner-path (path alice)
				   :time 0
				   :fn #'think))
	(run-rec (update-component csp act (id-of act))
		 evs)))))
