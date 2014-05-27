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

(defun plan (in &optional out)
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
	  (let ((dims-in (ccl::make-heap-ivector rank '(signed-byte 32))))
	    (loop for i below rank and j in (array-dimensions in) do
		 (setf (aref dims-in i) j))
	    )
	  #+sbcl
	  (let ((dims-in (make-array rank :element-type '(signed-byte 32)
				     :initial-contents (array-dimensions in))))
	   (with-pointer-to-vector-data (in-sap in-d)
	     (with-pointer-to-vector-data (out-sap out-d)
	       (with-pointer-to-vector-data (dims-sap dims-in)
		 (%fftw_plan_dft rank dims-sap in-sap out-sap
				 +forward+ +estimate+)))))))))

(defun ft (in)
  "Plan and execute an out-of-place Fourier transform of the array 'in'. The input array 'in' must be a displaced one-dimensional array."
  (declare (type (array (complex double-float) *) in))
  (let* ((out1 (make-array (array-total-size in) :element-type '(complex double-float)))
	 (out  (make-array (array-dimensions in) :element-type '(complex double-float)
			   :displaced-to out1))
	 (in1 (array-displacement in)))
    (progn ;if (and in1 (equal '(complex double-float) (array-element-type in)))
      (progn
	  (with-pointer-to-vector-data (in-sap in1)
	    #+sbcl (declare (ignore in-sap)) ;; i just do this in order to pin the array
	    (with-pointer-to-vector-data (out-sap out1)
	      #+sbcl (declare (ignore out-sap))
	      (let ((plan (plan in out)))
		(%fftw_execute plan))))
	  out)
      ; (error "input array is not displaced to 1d array. I can't work with this.")
      )))

