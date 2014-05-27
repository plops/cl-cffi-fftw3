(in-package :fftw)


#+linux
(progn
  (defcfun sysconf :long (name :int))
  (defparameter +_SC_NPROCESSORS_ONLN+ 84)
  (sysconf +_SC_NPROCESSORS_ONLN+))

(defun get-number-processors ()
  #+linux (let ((v (sysconf +_SC_NPROCESSORS_ONLN+)))
	    (if (= v -1)
		1
		v))
  #-linux 1)

(defun prepare-threads (&optional (n (get-number-processors)))
  "Initialize fftw3_threads to use n threads. On Linux n defaults to the number of processors."
  (%fftw_init_threads)
  (%fftw_plan_with_nthreads n))

(defun plan (in &optional out dims)
  (declare (type (array (complex double-float) *) in))
  (let* ((in-d (array-displacement in))
	 (out-d (array-displacement (if out out in))))
    (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
	(let* ((rank (array-rank in)))
	  ;;  http://ccl.clozure.com/ccl-documentation.html#Tutorial--Allocating-Foreign-Data-on-the-Lisp-Heap
	  ;; for clozure cl, foreign objects on the heap must be
	  ;; allocated on a special heap and freed after use. as far
	  ;; in my opinion the implementation of
	  ;; cffi:make-shareable-byte-vector in cffi 0.12.0 is
	  ;; actually wrong. it just returns an array produced by
	  ;; make-array. but it should use make-heap-ivector and later
	  ;; free it. however, as i understand, the semantics of cffi
	  ;; do not cover this.
	  
	  #+ccl
	  (progn
	    (when (and out (not (ccl::%heap-ivector-p out)))
	      (error "ccl only supports reusing the output array, if it is a heap-ivector"))
	    (let ((dims-in-foreign (ccl::make-heap-ivector rank '(signed-byte 32)))
		  (dims-in (if (ccl::%heap-ivector-p in)
			       dims
			       (array-dimensions in))))
	      (loop for i below rank and j in dims-in do
		   (setf (aref dims-in-foreign i) j))
	      
	      (let ((in-foreign (if (ccl::%heap-ivector-p in)
				    in
				    (ccl::make-heap-ivector (array-total-size in) '(complex double-float))))
		    (out-foreign (if (ccl::%heap-ivector-p out)
				    out
				    (ccl::make-heap-ivector (array-total-size in) '(complex double-float)))))

		(unless (ccl::%heap-ivector-p in) ;; only copy data, if in isn't already a heap ivector
		  (let ((in1 (make-array (array-total-size in) :element-type '(complex double-float)
					 :displaced-to in)))
		    (dotimes (i (length in1))
		      (setf (aref in-foreign i) (aref in1 i)))))
		(prog1
		    (values (%fftw_plan_dft rank dims-in-foreign in-foreign out-foreign
					    +forward+ +estimate+)
			    in-foreign out-foreign)
		  (ccl:dispose-heap-ivector dims-in-foreign))))
	    )
	  #+sbcl
	  (let ((dims-in (make-array rank :element-type '(signed-byte 32)
				     :initial-contents (array-dimensions in))))
	   (with-pointer-to-vector-data (in-sap in-d)
	     (with-pointer-to-vector-data (out-sap out-d)
	       (with-pointer-to-vector-data (dims-sap dims-in)
		 (%fftw_plan_dft rank dims-sap in-sap out-sap
				 +forward+ +estimate+)))))))))


(defparameter *bla* (ccl:make-heap-ivector (* 4 4 2) 'double-float))

(ccl:dispose-heap-ivector *bla*)

(make-array (list 4 4) :element-type 'double-float :displaced-to *bla*)

(defun ft (in &optional out)
  "Plan and execute an out-of-place Fourier transform of the array
'in'. SBCL allows to call the foreign function without copying the
data but the input array 'in' must be a displaced one-dimensional
array. In CCL, the input array and the output array are allocated on
the foreign heap, i.e. if you don't use ccl:make-heap-ivector to
allocate the arrays, the input and output data must be copied."
  (declare (type (array (complex double-float) *) in))
  (let* ((out1 (make-array (array-total-size in) :element-type '(complex double-float)))
	 (out  (make-array (array-dimensions in) :element-type '(complex double-float)
			   :displaced-to out1)))
    #+ccl
    (let ((in1 (make-array (array-total-size in) :element-type '(complex double-float)
			   :displaced-to in))
	  (in-foreign (if (ccl::%heap-ivector-p in)
			  in
			  (ccl::make-heap-ivector (array-total-size in) '(complex double-float))))
	  (out-foreign (if (ccl::%heap-ivector-p out)
			   out
			  (ccl::make-heap-ivector (array-total-size in) '(complex double-float)))))
      (dotimes (i (length in1))
	(setf (aref in-foreign i) (aref in1 i)))
      )
    #+sbcl
    (let ((in1 (array-displacement in)))
      (if (and in1 (equal '(complex double-float) (array-element-type in)))
	 (progn
	   (with-pointer-to-vector-data (in-sap in1)
	     (declare (ignore in-sap)) ;; i just do this in order to pin the array
	     (with-pointer-to-vector-data (out-sap out1)
	       (declare (ignore out-sap))
	       (let ((plan (plan in out)))
		 (%fftw_execute plan))))
	   out)
	 (error "input array is not displaced to 1d array. I can't work with this.")))
    ))

