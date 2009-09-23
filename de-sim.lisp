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


;(defpackage :it.unibo.cs.web.ritucci.de-sim
;  (:nicknames :de-sim)
;  (:use :common-lisp)
;  (:export))
;(in-package :de-sim)


;; errors

(define-condition not-found (error)
  nil)


(define-condition not-implemented (error)
  nil)


(define-condition empty-bag (error)
  nil)


(define-condition no-events (error)
  nil)


(define-condition no-destination (error)
  nil)


(define-condition access-denied (error)
  nil)


(define-condition access-temporarily-unavailable (error)
  nil)


;; restarts

(defun wait (c)
  (declare (ignore c))
  (invoke-restart 'wait))

(defun cancel (c)
  (declare (ignore c))
  (invoke-restart 'cancel))


(defgeneric clone (obj))
(defgeneric transform! (copy original))
(defgeneric setup-new! (obj))
(defgeneric connect! (bag-src bag-dst))
(defgeneric default-dest (sim bag))
(defgeneric access? (sim bag obj))
(defgeneric wakeup! (sim bag dst-bag dst-sim))
(defgeneric empty? (bag))
(defgeneric peek (bag))
(defgeneric insert! (bag obj &key))
(defgeneric remove! (bag &key))
(defgeneric lock! (bag))
(defgeneric unlock! (bag))
(defgeneric next-out-time (sim bag))
(defgeneric in! (sim bag obj dst-bag dst-sim))
(defgeneric out! (sim bag dst-bag dst-sim))
(defgeneric id= (obj obj))
(defgeneric size (obj))

;; Classes

;; Classe di base, tutte le altre DEVONO estenderla per avere l'id che
;; permette di riconoscere copie diverse dello stesso oggetto.
(defclass obj ()
  ((id       :initarg :id       :accessor id)
   ;; vero se l'oggetto deve essere considerato "morto"
   (dead?    :initarg :dead?    :accessor dead?)
   (ts       :initarg :ts       :accessor ts)      ; timestamp
   (name     :initarg :name     :accessor name     :type string)))


(defclass event (obj)
  ((owner-id :initarg :owner-id :accessor owner-id)
   ;; dead? = t significa: firato oppure annullato
   (tm       :initarg :tm       :accessor tm)
   (fn       :initarg :fn       :accessor fn)
   (desc     :initarg :desc     :accessor desc)))


(defclass sim (obj)
  nil)


(defclass bag (obj)
  ((owner    :initarg :owner    :accessor owner    :type sim)
   (lock?    :initarg :lock?    :accessor lock?    :type boolean)
   (waits?   :initarg :waits?   :accessor waits?   :type boolean)
   (waiting? :initarg :waiting? :accessor waiting? :type boolean)
   (elements :initarg :elements :accessor elements :type list)
   (sources  :initarg :sources  :accessor sources  :type list)
   (dests    :initarg :dests    :accessor dests    :type list)))


(defclass fbag (bag)
  ((flush?   :initarg :flush?   :accessor flush?   :type boolean)))


(defclass a2b-fbag (fbag)
  nil)

(defclass b2a-fbag (fbag)
  nil)


(defclass ln-> (sim)
  ((a2b      :initarg :a2b      :accessor a2b      :type a2b-fbag)
   (bw       :initarg :bw       :accessor bw)
   (err-rate :initarg :err-rate :accessor err-rate)
   (delay    :initarg :delay    :accessor delay)))


(defclass ln<-> (ln->)
  ((b2a      :initarg :b2a      :accessor b2a      :type b2a-fbag)))


;; utils

(defun usecs (us)
  us)

(defun msecs (ms)
  (* ms (usecs 1000)))

(defun secs (s)
  (* s (msecs 1000)))

(defun mins (m)
  (* m (secs 60)))


(defun bits (b)
  b)

(defun bytes (b)
  (* b (bits 8)))


(defun kilobits (kb)
  (* kb (bits (expt 10 3))))

(defun kilobytes (kb)
  (* kb (bytes (expt 10 3))))

(defun kibibits (kib)
  (* kib (bits (expt 2 10))))

(defun kibibytes (kib)
  (* kib (bytes (expt 2 10))))


(defun megabits (mb)
  (* mb (bits (expt 10 6))))

(defun megabytes (mb)
  (* mb (bytes (expt 10 6))))

(defun mebibits (mib)
  (* mib (bits (expt 2 20))))

(defun mebibytes (mib)
  (* mib (bytes (expt 2 20))))


(defun bits-per-second (bps)
  (/ (bits bps) (secs 1)))

(defun bytes-per-second (bps)
  (/ (bytes bps) (secs 1)))


(defun kilobits-per-second (kbps)
  (/ (kilobits kbps) (secs 1)))

(defun kilobytes-per-second (kbps)
  (/ (kilobytes kbps) (secs 1)))

(defun kibibits-per-second (kibps)
  (/ (kibibits kibps) (secs 1)))

(defun kibibytes-per-second (kibps)
  (/ (kibibytes kibps) (secs 1)))


(defun megabits-per-second (mbps)
  (/ (megabits mbps) (secs 1)))

(defun megabytes-per-second (mbps)
  (/ (megabytes mbps) (secs 1)))

(defun mebibits-per-second (mbps)
  (/ (mebibits mbps) (secs 1)))

(defun mebibytes-per-second (mbps)
  (/ (mebibytes mbps) (secs 1)))



(defparameter *id* -1)

(defun genid! ()
  (incf *id*))


(defun list-slots (instance)
  (mapcar #'sb-mop::slot-definition-name
	  (sb-mop::class-slots (class-of instance))))


(defun print-slots (object &optional (stream *standard-output*))
  (let ((slots (list-slots object)))
    (dolist (sl slots)
      (format stream
	      ":~a ~a~%"
	      (string-downcase sl)
	      (if (slot-boundp object sl)
		  (slot-value object sl)
		  "unbound")))))


(defun random-pick (lst &optional (random-state *random-state*))
  "Contract: list random-state -> element

   Purpose: to return one of the list element, choosing randomly."

  (declare (list lst))
  (nth (random (length lst)
	       random-state)
       lst))


(defun ht->list (ht)
  (declare (hash-table ht))
  (loop
     :for val :being :the :hash-values :in ht
     :collecting val))


(defmacro new (class-name &body body)
  `(setup-new! (make-instance ,class-name ,@body)))


(defmacro set-unbound-slots (instance &body forms)
  `(progn
    ,@(mapcar (lambda (sexp)
		(let ((slot-name (first sexp)))
		  `(when (not (slot-boundp ,instance ',slot-name))
		    (setf (slot-value ,instance ',slot-name)
		     ,(second sexp)))))
	      forms)
    nil))


(defmacro ! (&body body)
  `(progn
     ,@body
     nil))

;; OK, inserisco SIDE EFFECTS
;; questa servira' per l'invio dei pacchetti di rete, di cui viene
;; inviata una copia e l'originale resta nel buffer dell'interfaccia,
;; come nella realta'.
(defun modify! (obj &rest slot-value-plist)
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

(defmacro with-gensyms ((&rest names) &body body)
  "Stolen from Practical Common Lisp."
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))


;; METODI SIMULAZIONE


(defparameter *clock* 0)
(defparameter *evs* (list))

(defun gettime! ()
  *clock*)

(defun cancel! (ev)
  (! (setf (dead? ev) t)))

(defun change-time! (ev tm)
  (! (setf (tm ev) tm)
     (setf *evs* (stable-sort *evs* #'< :key #'tm))))

(defun fire! ()
  "Deve ritornare la lista di eventi generati da `event' oppure nil"
  (let ((ev (pop *evs*)))
    (if (null ev)
	(error 'no-events)
	(! (if (dead? ev)
	       (fire!)
	       (progn
		 (setf *clock* (tm ev))
		 (setf (dead? ev) t)
		 (format t "~a: ~a~%" *clock* ev)
		 (funcall (fn ev))))))))

(defun schedule! (ev)
  (declare (event ev))
  (! (setf *evs* (stable-sort (nconc *evs* (list ev))
			      #'< :key #'tm))))


(defmethod id= ((o1 obj) (o2 obj))
  (= (id o1)
     (id o2)))



(defmethod print-object ((ev event) stream)
  (print-unreadable-object (ev stream :type nil)
    (format stream "~a @~a: ~a" (name ev) (tm ev) (desc ev))))

;; METODI OBJ

(defmethod clone ((o obj))
  (let ((obj-copy (make-instance (class-of o))))
    (transform! obj-copy o)))

(defmethod transform! ((copy obj) (original obj))
  (dolist (slot-name (list-slots original) copy)
    (when (slot-boundp original slot-name)
      (setf (slot-value copy slot-name)
	    (slot-value original slot-name)))))


(defmethod print-object ((o obj) stream)
  (print-unreadable-object (o stream :type nil)
    (format stream "~a" (name o))))


(defmethod setup-new! ((o obj))
  (set-unbound-slots o
    (id (genid!))
    (dead? nil)
    (ts (gettime!))
    (name (concatenate 'string
		       (string (type-of o))
		       "-"
		       (write-to-string (id o)))))
  o)


(defmethod size ((o obj))
  (error 'not-implemented))


;; METODI BAG

(defmethod setup-new! ((b bag))
  (set-unbound-slots b
    (lock? nil)
    (waits? nil)
    (waiting? nil)
    (elements (list))
    (sources (list))
    (dests (list)))
  (call-next-method b))


(defmethod setup-new! ((b fbag))
  (set-unbound-slots b
    (flush? nil))
  (call-next-method b))


(defmethod connect! ((src bag) (dst bag))
  (! (push dst (dests src))
     (push src (sources dst))))


(defmethod default-dest ((s sim) (b bag))
  (if (null (dests b))
      (error 'no-destination)
      (first (dests b))))


(defmethod access? ((s sim) (b bag) (o obj))
  "Puo' sollevare gli errori `access-denied' e
  `access-temporarily-unavailable'"
  (if (lock? b)
      (error 'access-temporarily-unavailable)
      t))


(defmethod access? :around ((s sim) (b bag) (o obj))
  (restart-case (call-next-method s b o)
    (wait ()
      (! (setf (waits? b) t)))))


(defmethod empty? ((b bag))
  (null (elements b)))


(defmethod peek ((b bag))
  (if (empty? b)
      (restart-case (error 'empty-bag)
	(cancel ()
	  nil))
      (first (elements b))))


(defmethod insert! ((b bag) (o obj) &key)
  (! (setf (elements b)
	   (append (elements b) (list o)))))


(defmethod remove! ((b bag) &key)
  (if (empty? b)
      (restart-case (error 'empty-bag)
	(cancel ()
	  nil))
      (pop (elements b))))


(defmethod lock! ((b bag))
  (if (lock? b)
      (error "bag gia' lockata!")
      (! (setf (lock? b) t))))


(defmethod unlock! ((b bag))
  (when (not (lock? b))
    (error "bag gia' unlockata!"))
  (! (setf (lock? b) nil)
     (when (waits? b)
       (setf (waits? b) nil)
       (when (not (= 1 (length (sources b))))
	 (error "piu' di una source, come faccio a sapere quale sveglio!?"))
       (let* ((src (first (sources b)))
	      (src-sim (owner src)))
	 (setf (waiting? src) nil)
	 (schedule! (new 'event :tm (gettime!)
			 :desc (format nil "out! ~a ~a t t" src-sim src)
			 :fn (lambda ()
			       (out! (owner src) src t t))))))))


;; Tra un `out!' e il successivo `in!'  non ci deve essere tempo in
;; mezzo: altrimenti in quel tempo l'oggetto e' sospeso nel NULLA.
;; Ci deve essere tempo solo tra `in!' e il successivo `out'.

(defmethod next-out-time ((s sim) (b fbag))
  (gettime!))


(defmethod in! ((s sim) (b bag) (o obj) dst-bag dst-sim)
  (! (setf (ts o) (gettime!))
     (insert! b o)))


(defmethod in! ((s sim) (b fbag) (o obj) dst-bag dst-sim)
  (call-next-method)
  (when (not (flush? b))
    (setf (flush? b) t)
    (schedule! (new 'event :owner-id (id s)
		    :desc (format nil "out! ~a ~a ~a ~a" s b dst-bag dst-sim)
		    :tm (next-out-time s b)
		    :fn (lambda ()
			  (out! s b dst-bag dst-sim))))))


(defmethod out! ((s sim) (b bag) dst-bag dst-sim)
  (when (and (eq t dst-bag)
	     (eq t dst-sim))
    (setf dst-bag (default-dest s b))
    (setf dst-sim (owner dst-bag)))
  (if (not (access? dst-sim dst-bag (peek b)))
      (! (setf (waiting? b) t))
      (let ((o (remove! b)))
	(schedule!
	 (new 'event :tm (gettime!)
	      :desc (format nil "in! ~a ~a ~a ~a ~a" dst-sim dst-bag o t t)
	      :owner-id (id dst-sim)
	      :fn (lambda ()
		    (in! dst-sim dst-bag o t t)))))))


(defmethod out! ((s sim) (b fbag) dst-bag dst-sim)
  (call-next-method)
  (if (empty? b)
      (! (setf (flush? b) nil))
      (when (not (waiting? b))
	(schedule! (new 'event :owner-id (id s)
			:desc (format nil "out! ~a ~a t t" s b)
			:tm (next-out-time s b)
			:fn (lambda ()
			      ;; le destinazioni sono t t perche'
			      ;; devono essere decise nuovamente dal
			      ;; metodo specializzante.
			      (out! s b t t)))))))


;; METODI LN->

(defmethod setup-new! ((ln ln->))
  (set-unbound-slots ln
    (a2b (new 'a2b-fbag :owner ln))
    (bw :infinite)
    (err-rate 0)
    (delay 0))
  (call-next-method))


(defmethod setup-new! ((ln ln<->))
  (set-unbound-slots ln
    (b2a (new 'b2a-fbag :owner ln)))
  (call-next-method))


(defmethod next-out-time ((ln ln->) (b fbag))
  (let ((o (peek b)))
    (with-slots (bw delay) ln
      (let ((trans-time (+ delay (if (eql :infinite bw)
				     0
				     (/ (size o) bw)))))
	(+ (ts o)
	   trans-time)))))


(defmethod in! ((ln ln->) (b fbag) (o obj) dst-bag dst-sim)
  (lock! b)
  (schedule! (new 'event :owner-id (id b)
		  :desc (format nil "unlock! ~a" b)
		  :tm (+ (gettime!)
			 (if (eql :infinite (bw ln))
			     0
			     (/ (size o) (bw ln))))
		  :fn (lambda ()
			(unlock! b))))
  (when (not (< (random 100)
		(err-rate ln)))
    (call-next-method)))
