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
  (init-threads)
  (plan-with-nthreads n))

(defun plan (in &optional out)
  (declare (type (array (complex double-float) *) in))
  (let* ((in-d (array-displacement in))
	 (out-d (array-displacement (if out out in))))
    (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
	(let* ((rank (array-rank in))
	       (dims-in (make-array rank :element-type '(signed-byte 32)
				    :initial-contents (array-dimensions in))))
	  (format t "array alignment ~a" (list in-sap out-sap))
	  ;; this should work for 
	  (with-pointer-to-vector-data (in-sap in-d)
	    (with-pointer-to-vector-data (out-sap out-d)
	      (with-pointer-to-vector-data (dims-sap dims-in)
		(fftw_plan_dft rank dims-sap in-sap out-sap
			       +forward+ +estimate+))))))))

(defun ft (in)
  (declare (type (array (complex double-float) *) in))
  (let* ((out1 (make-array (reduce #'* (array-dimensions in))
			   :element-type '(complex double-float)))
	 (out (make-array (array-dimensions in)
			  :element-type '(complex double-float)
			  :displaced-to out1)))
    (if (and (array-displacement in)
	     (equal '(complex double-float) (array-element-type in)))
	(with-pointer-to-vector-data (in-sap in) ;; i just do this in order to pin the array
	  (with-pointer-to-vector-data (out-sap out)
	    (let ((plan (plan in out)))
	      (fftw_execute plan))))
	(format t "input array is not displaced to 1d array. I can't work with this"))
    out))

