(in-package :with-c)

(defcstruct point
  (x :int )
  (y :int ))

(defparameter point (cffi::parse-type '(:struct point)))

(defun test1 ()
  (with-c (:temp point p1)
    (setf x 1
	  y 2)
    (format t "point ~A ~A" x y)))

(defun test2 ()
  (multiple-value-bind (c1 c2)
      
      (with-c ((:new point p1 "A")
	       (:new point p2 "B"))
	(setf ax 1
	      ay 2
	      bx 3
	      by 4)
	(format t "~%point p1 ~A ~A" ax ay)
	(format t "~%point p2 ~A ~A" bx by)
	(values p1 p2))
    ;; and the old-fashioned way
    (format t "~%point p1 ~A ~A"
	    (mem-ref c1 :int 0)
	    (mem-ref c1 :int 4))
    (format t "~%point p2 ~A ~A"
	    (mem-ref c2 :int 0)
	    (mem-ref c2 :int 4))
    (foreign-free c1)
    (foreign-free c2)))
