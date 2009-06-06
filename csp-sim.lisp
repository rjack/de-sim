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
;; by a string (let's call it wire from now on).  Talk into a can and
;; the wire will transmit the sound to the other can.
;;
;; Components:
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


(defclass can (actor)
  nil)


(defclass wire (actor)
  nil)


(defclass csp-scenario (simulator)
  ((cans
    :initarg :cans
    :initform (error ":cans missing")
    :reader cans-of
    :type (cons can can))
   (wire
    :initarg :wire
    :initform (error ":wire missing")
    :reader wire-of
    :type wire)))
