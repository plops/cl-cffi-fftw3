(declaim (optimize (debug 3) (safety 3) (speed 0)))
#+nil
(require 'asdf)
#+nil
(load "~/quicklisp/setup.lisp")
#+nil
(load "c:/users/minyi_000/quicklisp/setup.lisp")
#+nil
(ql:quickload 'cffi)
#+nil
(ql:quickload 'trivial-garbage)



(setf asdf:*central-registry*
   ;; Default directories, usually just the ``current directory''
  '(*default-pathname-defaults*

    ;; Additional places where ASDF can find
    ;; system definition files
    #+linux #p"/home/martin/stage/cl-cffi-fftw3/"
    #-linux #p"d:/martin/cl-cffi-fftw3/"))
#+nil
(asdf:load-system "fftw")



(fftw:prepare-threads)

(let* ((w 15)
       (h 14)
       ;; allocate a 1d array
       #+nil (a1 (make-array (* w h) :element-type '(complex double-float)))
       ;; create a 2d array for access
       (a (fftw:make-foreign-complex-array-as-double (list h w))
	 #+nil (make-array (list h w) :element-type '(complex double-float)
			 :displaced-to a1))
       (b (fftw:make-foreign-complex-array-as-double (list h w))
	 #+nil (make-array (list h w) :element-type '(complex double-float)
			 :displaced-to a1)))
  
  ;; fill the 2d array with a sinosoidal grating    		    
  (dotimes (i w)
    (dotimes (j h)
      #+nil (setf (aref a j i) (complex (sin (* 8 pi (+ (/ i w) (/ j h))))))
      (setf (aref a j i 0) (sin (* 8 pi (+ (/ i w) (/ j h))))
	    (aref a j i 1) 0d0)))

  ;; call fftw
  
  (time
   (defparameter *bla* (fftw:ft a b)))

  ;; print out each element of the array. scale data to lie within 0..9
  (progn
    (terpri)
    (destructuring-bind (h w two) (array-dimensions *bla*)
      (dotimes (j h)
	(dotimes (i w)
	  (format t "~1,'0d" (floor (abs (complex (aref *bla* j i 0) (aref *bla* j i 1))) (/ (* h w) 9))))
	(terpri)))))


