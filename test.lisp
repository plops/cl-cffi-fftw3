(declaim (optimize (debug 3) (safety 3) (speed 0)))
#+nil
(setf asdf:*central-registry*
   ;; Default directories, usually just the ``current directory''
  '(*default-pathname-defaults*

    ;; Additional places where ASDF can find
    ;; system definition files
    #p"/home/martin/stage/cl-cffi-fftw3/"))
#+nil
(asdf:load-system "fftw")

(fftw:prepare-threads)

(let* ((w 37)
       (h 12)
       (a1 (make-array (* w h) :element-type '(complex double-float)))
       (a (make-array (list h w) :element-type '(complex double-float)
		      :displaced-to a1)))
  (dotimes (i w)
    (dotimes (j h)
      (setf (aref a j i) (complex (sin (* 8 pi (+ (/ i w) (/ j h))))))))
  (defparameter *bla* (fftw:ft a))
  (progn
  (terpri)
  (destructuring-bind (h w) (array-dimensions *bla*)
    (dotimes (j h)
      (dotimes (i w)
	(format t "~1,'0d" (floor (abs (aref *bla* j i)) (/ (* h w) 9))))
      (terpri)))))


