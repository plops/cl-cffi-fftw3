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

(fftw:prepare-threads 2)
#+nli
(fftw::%fftw_plan_with_nthreads 2)
#+nil
(fftw::%sfftw_plan_with_nthreads 2)

#+nil
(sb-alien::dlsym sb-alien::*runtime-dlhandle* "fftw_plan_with_nthreads")

(defparameter *bla* nil)
(sb-ext:gc :full t)
(room)

(time
 (let* ((n 64)
	(a n)
	(b n)
	(c n)
	(d n)
	(q1 (make-array (* a b c d) :element-type '(complex double-float)))
	(p1 (make-array (* a b c d) :element-type '(complex double-float)))
	(q (make-array (list a b c d) :element-type '(complex double-float)
		       :displaced-to q1))
	(p (make-array (list a b c d) :element-type '(complex double-float)
		       :displaced-to p1)))
   (fftw:ft q :out-arg p :flag fftw::+measure+)
   nil))

(time
 (let* ((n 512)
	(a n)
	(b n)
	
	(q1 (make-array (* a b ) :element-type '(complex double-float)))
	(p1 (make-array (* a b ) :element-type '(complex double-float)))
	(q (make-array (list a b ) :element-type '(complex double-float)
		       :displaced-to q1))
	(p (make-array (list a b ) :element-type '(complex double-float)
		       :displaced-to p1)))
   (sb-sys:with-pinned-objects (p q p1 q1)
     (let ((plan (fftw::plan q :out p :flag fftw::+patient+)))
       (time (dotimes (i 100) (fftw::%fftw_execute plan)))))
   nil))
;; 0.349s on 2 threads x201 Intel(R) Core(TM) i5 CPU       M 520  @ 2.40GHz

(time
 (let* ((n 512)
	(a n)
	(b n)
	(bo (+ 1 (floor n 2)))
	(q1 (make-array (* a b ) :element-type 'single-float))
	(p1 (make-array (* a bo ) :element-type '(complex single-float)))
	(q (make-array (list a b ) :element-type 'single-float
		       :displaced-to q1))
	(p (make-array (list a bo ) :element-type '(complex single-float)
		       :displaced-to p1)))
   (sb-sys:with-pinned-objects (p q p1 q1)
     (let ((plan (fftw::srplan q :out p :flag fftw::+patient+)))
       (time (dotimes (i 100) (fftw::%sfftw_execute plan)))))
   nil))


(/ (* 16d0 (expt 84 4))
   (expt 1024 2))

(let* ((w 15)
       (h 14)
       ;; allocate a 1d array
       #+sbcl (a1 (make-array (* w h) :element-type '(complex double-float)))
       #+sbcl (b1 (make-array (* w h) :element-type '(complex double-float)))
       ;; create a 2d array for access
       (a #+ccl (fftw:make-foreign-complex-array-as-double (list h w))
	  #+sbcl (make-array (list h w) :element-type '(complex double-float)
			 :displaced-to a1))
       (b #+ccl (fftw:make-foreign-complex-array-as-double (list h w))
	  #+sbcl (make-array (list h w) :element-type '(complex double-float)
			     :displaced-to b1)))
  
  ;; fill the 2d array with a sinosoidal grating    		    
  (dotimes (i w)
    (dotimes (j h)
      #+sbcl (setf (aref a j i) (complex (sin (* 8 pi (+ (/ i w) (/ j h))))))
      #+ccl (setf (aref a j i 0) (sin (* 8 pi (+ (/ i w) (/ j h))))
		  (aref a j i 1) 0d0)))

  ;; call fftw
  (defparameter *bla* (fftw:ft a b))

  ;; print out each element of the array. scale data to lie within 0..9
  (progn
    (terpri)
    #+ccl
    (destructuring-bind (h w two) (array-dimensions *bla*)
      (dotimes (j h)
	(dotimes (i w)
	  (format t "~1,'0d" (floor (abs (complex (aref *bla* j i 0) (aref *bla* j i 1))) (/ (* h w) 9))))
	(terpri)))
    (destructuring-bind (h w) (array-dimensions *bla*)
      (dotimes (j h)
	(dotimes (i w)
	  (format t "~1,'0d" (floor (abs (aref *bla* j i)) (/ (* h w) 9))))
	(terpri)))))


