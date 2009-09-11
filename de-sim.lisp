;; DE-SIM
;; Discrete Event SIMulator.

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


(defpackage :it.unibo.cs.web.ritucci.de-sim
  (:nicknames :de-sim)
  (:use :common-lisp)
  (:export))
(in-package :de-sim)


;; errors

(define-condition not-found (error)
  nil)


(define-condition not-implemented (error)
  nil)


;; Classes

;; Classe di base, tutte le altre DEVONO estenderla per avere l'id che
;; permette di riconoscere copie diverse dello stesso oggetto.
(defclass obj ()
  ((id       :initarg :id       :accessor id)
   ;; vero se l'oggetto deve essere considerato "morto"
   (dead     :initarg :dead     :accessor dead)))


(defclass event (obj)
  ((owner-id :initarg :owner-id :accessor owner-id)
   (tm       :initarg :tm       :accessor tm)
   (fn       :initarg :fn       :accessor fn)))


(defclass sim (obj)
  ((tm       :initarg :tm       :accessor tm)))


(defclass bag (obj)
  ((owner    :initarg :owner    :accessor owner   :type sim)
   (lock     :initarg :lock     :accessor lock    :type boolean)
   (elements :initarg :elements :accessor elements)
   (sources  :initarg :sources  :accessor sources :type list)
   (dests    :initarg :dests    :accessor dests   :type list)))


(defclass scenario (sim)
  nil)


(defclass ln-> (sim)
  ((a-id     :initarg :a-id     :accessor a-id)
   (a2bq     :initarg :a2bq     :accessor a2bq :type bag)
   (bw       :initarg :bw       :accessor bw)
   (err-rate :initarg :err-rate :accessor err-rate)
   (delay    :initarg :delay    :accessor delay)))


(defclass ln<-> (ln->)
  ((b-id     :initarg :b-id     :accessor b-id)
   (b2aq     :initarg :b2aq     :accessor b2aq :type bag)))


(defclass ln<=> (ln<->)
  nil)


;; utils


(let ((id -1))

  (defun genid ()
    (incf id)))


(defun list-slots (instance)
  (mapcar #'sb-mop::slot-definition-name
	  (sb-mop::class-slots (class-of instance))))


(defun ht->list (ht)
  (declare (hash-table ht))
  (loop
     :for val :being :the :hash-values :in ht
     :collecting val))


(defmacro new (class-name &body body)
  `(setup-new (make-instance ,class-name ,@body)))


;; OK, inserisco SIDE EFFECTS
;; questa servira' per l'invio dei pacchetti di rete, di cui viene
;; inviata una copia e l'originale resta nel buffer dell'interfaccia,
;; come nella realta'.
(defun modify (obj &rest slot-value-plist)
  "Return the modified instance of obj.
   Example: class A has slots FOO and BAR.
            (setf *obj* (make-instance 'a :foo 'foo :bar 'bar))
            (modify *org*)  ; nothing changed
            (modify *org* :foo 'new-foo)  ; change foo
            (modify *org* :foo 'new-foo :bar 'new-bar)"
  ;; Set the given value for each slot specified in
  ;; slot-value-plist.
  (do* ((s/v slot-value-plist (cddr s/v))
	(slot-kwd (first s/v) (first s/v))
	(value (second s/v) (second s/v)))
       ((null slot-kwd) obj)
    (let ((slot-symbol (intern (string slot-kwd))))
      (setf (slot-value obj slot-symbol)
	    value))))


(defun clone (obj &rest slot-value-plist)
  (let ((obj-copy (apply #'modify
			 (make-instance (class-of obj))
			 slot-value-plist)))
    (dolist (slot-name (list-slots obj) obj-copy)
      (when (and (slot-boundp obj slot-name)
		 (not (slot-boundp obj-copy slot-name)))
	(setf (slot-value obj-copy slot-name)
	      (slot-value obj slot-name))))))


(defmacro with-gensyms ((&rest names) &body body)
  "Stolen from Practical Common Lisp."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


;; METODI


(defmethod setup-new :around ((o obj))
  (with-slots (id dead) o
    (setf id (genid))
    (setf dead nil))
  (call-next-method))


(defmethod setup-new ((s sim))
  "Crea i componenti di sim e i link tra di essi. Da specializzare."
  s)


;; BAGS

(defmethod setup-new :around ((b bag))
  (with-slots (lock elements sources dests) b
    (setf lock nil)
    (setf elements (list))
    (setf sources (list))
    (setf dests (list)))
  (call-next-method))


(defmethod connect ((src bag) (dst bag))
  (push dst (dests src))
  (push src (sources dst)))


(defmethod fire ((s sim) (ev event))
  (funcall (fn ev)
	   (modify s :tm (tm ev))))


(defmethod id= ((s1 sim) (s2 sim))
  (= (id s1) (id s2)))
