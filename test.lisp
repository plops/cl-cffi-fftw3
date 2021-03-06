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
#+nil
(fftw:prepare-threads 6)
#+nli
(fftw::%fftw_plan_with_nthreads 4)
#+nil
(fftw::%fftwf_plan_with_nthreads 4)

#+nil
(fftw::%fftw_export_wisdom_to_filename "/home/martin/fftw-x201.wisdom")
#+nil
(fftw::%fftw_export_wisdom_to_filename "/home/martin/fftw-bluechip_4.wisdom")
#+nil
(fftw::%fftw_export_wisdom_to_filename "/home/martin/fftw-fiberholo.wisdom")
#+nil
(fftw::%fftw_import_wisdom_from_filename "/home/martin/fftw-x201.wisdom")
#+nil
(fftw::%fftw_import_wisdom_from_filename "/home/martin/fftw-bluechip_4.wisdom")
#+nil
(fftw::%fftwf_export_wisdom_to_filename "/home/martin/fftwf-x201.wisdom")

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
;; 0.371s on 2 threads x201 Intel(R) Core(TM) i5 CPU       M 520  @ 2.40GHz after recompiling gentoo fftw with avx, meas
;; 0.184s on 2 threads bluechip Intel(R) Core(TM) i5-4440 CPU @ 3.10GHz (total .36s)
;; 0.112s on 4 threads bluechip Intel(R) Core(TM) i5-4440 CPU @ 3.10GHz (total .42s)
;; for i in `find /sys|grep scaling_gover` ;do echo performance > $i;done
;; 0.196 on 6 threads fiberholo AMD Phenom(tm) II X6 1055T Processor  (total 1.14s)
;; 0.273 (but fluctuates to .5) on 3 threads fiberholo AMD Phenom(tm) II X6 1055T Processor  (total .793s)

(time
 (let* ((n 580)
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
;; n=512
;; 0.349s on 2 threads x201 Intel(R) Core(TM) i5 CPU       M 520  @ 2.40GHz
;; 0.429s on 2 threads x201 Intel(R) Core(TM) i5 CPU       M 520  @ 2.40GHz after recompiling gentoo fftw with avx
;; 0.059s on 4 threads bluechip Intel(R) Core(TM) i5-4440 CPU @ 3.10GHz (total .228s)
;; 0.156s on 2 threads bluechip Intel(R) Core(TM) i5-4440 CPU @ 3.10GHz (total .304s)
;; 0.204 on 6 threads fiberholo AMD Phenom(tm) II X6 1055T Processor  (total 1.17s)

;; n=580
;; 1.105s 2 threads x201 Intel(R) Core(TM) i5 CPU       M 520  @ 2.40GHz after recompiling gentoo fftw with avx
;; 1.068s 4 threads 
;; 0.303s on 4 threads bluechip Intel(R) Core(TM) i5-4440 CPU @ 3.10GHz (total 1.184s)
;; 0.380s on 6 threads fiberholo AMD Phenom(tm) II X6 1055T Processor  (total 2.202s) 
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
     (let ((plan (fftw::rplanf2 q :out p :flag fftw::+estimate+)))
       (time (dotimes (i 100) (fftw::%fftwf_execute plan)))))
   nil))

(let* ((n 512)
	(no (+ 1 (floor n 2)))
	(i1 (make-array n :element-type 'double-float))
	(o1 (make-array no :element-type '(complex double-float))))
   (sb-sys:with-pinned-objects (i1 o1)
     (let ((plan (fftw::rplan i1 :out o1 :flag fftw::+patient+)))
       (time (dotimes (i 1000) (fftw::%fftw_execute plan)))))
   nil)

(let* ((n 512) ;; 1d call
       (no (+ 1 (floor n 2)))
       (i1 (make-array n :element-type 'double-float))
       (o1 (make-array no :element-type '(complex double-float))))
  (sb-sys:with-pinned-objects (i1 o1)
    (fftw::%fftw_plan_dft_r2c_1d 512
				 (sb-sys:vector-sap i1)
				 (sb-sys:vector-sap o1)
				 fftw::+forward+ fftw::+measure+
				 )))

(let* ((n 512) ;; 2d call
       (no (+ 1 (floor n 2)))
       (i1 (make-array (list n n) :element-type 'single-float))
       (o1 (make-array (list n no) :element-type '(complex single-float))))
  (sb-sys:with-pinned-objects (i1 o1)
    (let ((plan (fftw::rplanf i1 :out o1 :flag fftw::+patient+)))
      (time (dotimes (i 1000) (fftw::%fftw_execute plan))))))
;; n=512
;; 0.241s 4 threads bluechip Intel(R) Core(TM) i5-4440 CPU @ 3.10GHz (total .92s)
;; 0.651s on 6 threads fiberholo AMD Phenom(tm) II X6 1055T Processor  (total 3.4s) 

;; n=580
;; 1.49s 4 threads bluechip Intel(R) Core(TM) i5-4440 CPU @ 3.10GHz (total 5.832s)
;; 1.85s on 6 threads fiberholo AMD Phenom(tm) II X6 1055T Processor  (total 10.81s) 
(sb-sys:with-pinned-objects (in-d out-d dims-in)
  (let ((r ))
    (when (cffi:null-pointer-p r)
		  (error "plan_dft_r2c didn't succeed."))
		r))


(let* ((n 512)
	(no (+ 1 (floor n 2)))
	(i1 (make-array n :element-type 'single-float))
	(o1 (make-array no :element-type '(complex single-float))))
   (sb-sys:with-pinned-objects (i1 o1)
     (let ((plan (fftw::rplanf i1 :out o1 :flag fftw::+patient+)))
       (time (dotimes (i 1000) (fftw::%fftwf_execute plan)))))
   nil)

(let* ((n 512)
       (i1 (make-array n :element-type '(complex single-float)))
       (o1 (make-array n :element-type '(complex single-float))))
   (sb-sys:with-pinned-objects (i1 o1)
     (let ((plan (fftw::planf i1 :out o1 :flag fftw::+patient+)))
       (time (dotimes (i 100000) (fftw::%fftwf_execute plan)))))
   nil)



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


