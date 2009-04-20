(defmethod add ((buf buffer) (it item))
  (if (fits? buf it)
      (setf (elements-of buf)
	    (append (elements-of buf)
		    (list it)))
      (error "item does not fit in buffer")))


(defmethod notify-add ((buf buffer))
  (when (idle? buf)
    (output-begin buf)))


(defmethod output-begin ((buf buffer) (po port))
  (labels ((mark-busy (buf)
	     (setf (idle-flag-of buf) nil)
	     buf)
	   (stage-elem (buf)
	     (setf (output-in-progress-of buf)
		   (pop (elements-of buf)))
	     buf))
    (will (lambda ()
	    (output-end buf po))
	  :relative (i/o-duration (stage-elem (mark-busy buf))))))


(defmethod output-end ((buf buffer) (po port)))