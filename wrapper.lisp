(in-package :fftw)

(defcfun sysconf :long (name :int))
(defparameter +_SC_NPROCESSORS_ONLN+ 84)
(sysconf +_SC_NPROCESSORS_ONLN+)

(defun get-number-processors ()
  (let ((v (sysconf +_SC_NPROCESSORS_ONLN+)))
    (if (= v -1)
	1
	v)))

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
	(let* ((rank (array-rank in))
	       (dims-in (make-array rank :element-type '(signed-byte 32)
				    :initial-contents (array-dimensions in))))
	  ;; this should work for 
	  (with-pointer-to-vector-data (in-sap in-d)
	    (with-pointer-to-vector-data (out-sap out-d)
	      (with-pointer-to-vector-data (dims-sap dims-in)
		(%fftw_plan_dft rank dims-sap in-sap out-sap
			       +forward+ +estimate+))))))))

(defun ft (in)
  "Plan and execute an out-of-place Fourier transform of the array 'in'. The input array 'in' must be a displaced one-dimensional array."
  (declare (type (array (complex double-float) *) in))
  (let* ((out1 (make-array (array-total-size in) :element-type '(complex double-float)))
	 (out  (make-array (array-dimensions in) :element-type '(complex double-float)
			   :displaced-to out1))
	 (in1 (array-displacement in)))
    (if (and in1 (equal '(complex double-float) (array-element-type in)))
	(progn
	 (with-pointer-to-vector-data (in-sap in1)
	   (declare (ignorable in-sap)) ;; i just do this in order to pin the array
	   (with-pointer-to-vector-data (out-sap out1)
	     (declare (ignorable out-sap))
	     (let ((plan (plan in out)))
	       (%fftw_execute plan))))
	 out)
	(error "input array is not displaced to 1d array. I can't work with this."))))

