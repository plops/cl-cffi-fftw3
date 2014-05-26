

(defconstant +forward+ -1)
(defconstant +backward+ 1)
(defconstant +measure+ 0)
(defconstant +estimate+ (ash 1 6)) ;; array isn't overwritten during planning

(load-shared-object "libfftw3.so.3")
(define-alien-type plan (* int))

(define-alien-routine fftw_execute
    void
  (plan plan))

(define-alien-routine fftw_destroy_plan
    void
  (plan plan))

(define-alien-routine fftw_plan_dft
    plan
  (rank int)
  (n (* int))
  (in (* double-float))	 ;; actually complex
  (out (* double-float))	  ;; actually complex
  (sign int)
  (flags unsigned-int))


(load-shared-object "libfftw3_threads.so.3")
  
(define-alien-routine ("fftw_init_threads" init-threads)
    int)

(define-alien-routine ("fftw_plan_with_nthreads" plan-with-nthreads)
    void
  (nthreads int))


(progn
 (init-threads)
 (plan-with-nthreads 4))


(defun plan (in &optional out)
  (declare (type (array (complex double-float) *) in))
  (let* ((in-d (array-displacement in))
	 (out-d (array-displacement (if out out in))))
    (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
     (let* ((rank (array-rank in))
	    (dims-in (make-array rank :element-type '(signed-byte 32)
				 :initial-contents (array-dimensions in)))
	    (in-sap (sb-sys:vector-sap
		     (sb-ext:array-storage-vector in-d)))
	    (out-sap (sb-sys:vector-sap
		      (sb-ext:array-storage-vector out-d))))
       (format t "array alignment ~a" (list in-sap out-sap))
       (sb-sys:with-pinned-objects (dims-in in out)
	 (fftw_plan_dft rank (sb-sys:vector-sap dims-in)
			in-sap out-sap +forward+ +estimate+))))))

(defun ft (in)
  (declare (type (array (complex double-float) *) in))
  (let* ((out1 (make-array (reduce #'* (array-dimensions in))
			   :element-type '(complex double-float)))
	 (out (make-array (array-dimensions in)
			  :element-type '(complex double-float)
			  :displaced-to out1)))
    (if (and (array-displacement in)
	     (equal '(complex double-float) (array-element-type in)))
	(sb-sys:with-pinned-objects (in out)
	  (let ((plan (plan in out)))
	    (fftw_execute plan)))
	(let* ((a1 (make-array (reduce #'* (array-dimensions in))
			       :element-type '(complex double-float)))
	       (a (make-array (array-dimensions in)
			      :element-type '(complex double-float)
			      :displaced-to a1))
	       (in1 (sb-ext:array-storage-vector in))
	       (in-type (array-element-type in1)))
	  (format t "input array is not displaced to 1d array, I will make a copy.")
	  (cond
	    ((eq 'double-float in-type)
	     (format t "input array is not of complex double-float type. I will convert it.")
	     (dotimes (i (length a1))
	       (setf (aref a1 i) (complex (aref in i)))))
	    ((equal '(complex double-float) in-type)
	     (dotimes (i (length a1))
	       (setf (aref a1 i) (aref in i))))

	    (t (format t "input array has an unsupported element type.")))
	  (sb-sys:with-pinned-objects (a out)
	   (let ((plan (plan a out)))
	     (fftw_execute plan)))))
    out))
