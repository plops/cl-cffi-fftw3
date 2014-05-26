(declaim (optimize (debug 3) (safety 3) (speed 0)))
#+nil
(require 'asdf)
#+nil
(load "~/quicklisp/setup.lisp")
#+nil
(ql:quickload 'cffi)
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
       ;; allocate a 1d array
       (a1 (make-array (* w h) :element-type '(complex double-float)))
       ;; create a 2d array for access
       (a (make-array (list h w) :element-type '(complex double-float)
		      :displaced-to a1)))
  
  ;; fill the 2d array with a sinosoidal grating    		    
  (dotimes (i w)
    (dotimes (j h)
      (setf (aref a j i) (complex (sin (* 8 pi (+ (/ i w) (/ j h))))))))

  ;; call fftw
  (defparameter *bla* (fftw:ft a))

  ;; print out each element of the array. scale data to lie within 0..9
  (progn
    (terpri)
    (destructuring-bind (h w) (array-dimensions *bla*)
      (dotimes (j h)
	(dotimes (i w)
	  (format t "~1,'0d" (floor (abs (aref *bla* j i)) (/ (* h w) 9))))
	(terpri)))))
