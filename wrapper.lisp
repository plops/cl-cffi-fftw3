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
  (when (= 0 (%fftw_init_threads))
    (error "prepare-threads: error by fftw_init_threads"))
  (when (= 0 (%fftwf_init_threads))
    (error "prepare-threads: error by fftwf_init_threads"))
  ;; fixme occasionally i should call fftw_cleanup_threads
  (%fftw_plan_with_nthreads n)
  (%fftwf_plan_with_nthreads n))

(defun plan (in &key out w h (flag +estimate+) (sign +forward+))
  "Plan a Fast fourier transform. If in and out are given,
out-of-place transform. If only one is given, in-place transform."
  #+sbcl (declare (type (array (complex double-float) *) in))
  (let* ((in-d (array-displacement in))
	 (out-d (array-displacement (if out out in))))
   (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
	(let* ((dims #+sbcl (array-dimensions in)
		     #+ccl (butlast (array-dimensions in)))
	       (rank #+sbcl (array-rank in) 
		     #+ccl (if (foreign-complex-array-as-double-p in)
			       (1- (array-rank in))
			       (error "please give plan an array that has been allocated on the foreign heap."))))
	  ;;  http://ccl.clozure.com/ccl-documentation.html#Tutorial--Allocating-Foreign-Data-on-the-Lisp-Heap
	  ;; for clozure cl, foreign objects on the heap must be
	  ;; allocated on a special heap and freed after use. as far
	  ;; in my opinion the implementation of
	  ;; cffi:make-shareable-byte-vector in cffi 0.12.0 is
	  ;; actually wrong. it just returns an array produced by
	  ;; make-array. but it should use make-heap-ivector and later
	  ;; free it. however, as i understand, the semantics of cffi
	  ;; do not cover this (note: i now think one could implement
	  ;; this using ccl:terminate-when-unreachable).
	  
	  #+ccl
	  (progn
	    (when (and out (not (foreign-complex-array-as-double-p out)))
	      (error "ccl only supports reusing the output array, if it was allocated as a heap-ivector"))
	    (multiple-value-bind (dims-in-foreign dims-p) (ccl::make-heap-ivector rank '(signed-byte 32))
	      (loop for i below rank and j in dims do
		   (setf (aref dims-in-foreign i) j))
	      (let ((in-p (ccl:%null-ptr))
		    (out-p (ccl:%null-ptr)))
		(ccl::%vect-data-to-macptr in-d in-p)
		(ccl::%vect-data-to-macptr out-d out-p)
		(prog1 
		    (%fftw_plan_dft rank dims-p in-p out-p +forward+ +estimate+)
		  (ccl:dispose-heap-ivector dims-in-foreign)))))
	  #+sbcl
	  (let ((dims-in (make-array rank :element-type '(signed-byte 32)
				     :initial-contents (or (and (and w h) (list h w))
							   dims))))
	    (sb-sys:with-pinned-objects (in-d out-d dims-in)
	      (let ((r (%fftw_plan_dft rank (sb-sys:vector-sap dims-in) (sb-sys:vector-sap in-d)
				       (sb-sys:vector-sap out-d)
				       sign flag)))
		(when (cffi:null-pointer-p r)
		  (error "plan_dft didn't succeed."))
		r)))))))

(defun rplan (in &key out w h (flag +estimate+) (sign +forward+))
  "Plan a Fast fourier transform with real input of double float. If in and out are given, out-of-place transform. In-place transform is not supported (because it would need padding)."
  (declare (type (array double-float *) in))
  (unless out
    (error "in-place transform not supported."))
  (let* ((in-d (or (array-displacement in) in))
	 (out-d (or (array-displacement out) out)))
    (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
	(let* ((dims (array-dimensions in))
	       (rank (array-rank in)))
	  (let* ((dims-l (or (and (and w h) (list h w))
			    dims))
		 (dims-in (make-array rank :element-type '(signed-byte 32)
				      :initial-contents dims-l)))
	    (assert (<= (* (reduce #'* (butlast dims-l)) 
			   (+ 1 (floor (first (last dims-l)) 2)))
			(array-total-size out-d)))
	    (sb-sys:with-pinned-objects (in-d out-d dims-in)
	      (let ((r (%fftw_plan_dft_r2c  rank
					   (sb-sys:vector-sap dims-in)
					   (sb-sys:vector-sap in-d)
					   (sb-sys:vector-sap out-d)
					   flag)))
		(when (cffi:null-pointer-p r)
		  (error "plan_dft_r2c didn't succeed."))
		r)))))))

(defun rplanf (in &key out w h (flag +estimate+))
  "Plan a Fast fourier transform with real input of single float. If in and out are given, out-of-place transform. In-place transform is not supported (because it would need padding)."
  ;(declare (type (array single-float *) in))
  (unless out
    (error "in-place transform not supported."))
  (let* ((in-d (or (array-displacement in) in))
	 (out-d (or (array-displacement out) out)))
    (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
	(let* ((dims (array-dimensions in))
	       (rank (array-rank in)))
	  (let* ((dims-l (or (and (and w h) (list h w))
			    dims))
		 (dims-in (make-array rank :element-type '(signed-byte 32)
				      :initial-contents dims-l)))
	    (assert (<= (* (reduce #'* (butlast dims-l)) 
			   (+ 1 (floor (first (last dims-l)) 2)))
			(array-total-size out-d)))
	    (sb-sys:with-pinned-objects (in-d out-d dims-in)
	      (let ((r (%fftwf_plan_dft_r2c rank
					    (sb-sys:vector-sap dims-in)
					    (sb-sys:vector-sap (sb-ext:array-storage-vector in-d))
					    (sb-sys:vector-sap (sb-ext:array-storage-vector out-d))
					    flag)))
		(when (cffi:null-pointer-p r)
		  (error "plan_dft_r2c didn't succeed."))
		r)))))))

(defun rplanf2 (in &key out w h (flag +estimate+) (sign +forward+))
  "Plan a Fast fourier transform with 2d real input of single float. If in and out are given, out-of-place transform. In-place transform is not supported (because it would need padding)."
  (declare (type (array single-float 2) in))
  (unless out
    (error "in-place transform not supported."))
  (let* ((in-d (or (array-displacement in) in))
	 (out-d (or (array-displacement out) out)))
    (if (not (and in-d out-d))
	(error "initially you should allocate data as a 1d array in lisp and then use displacement.")
	(let* ((dims (array-dimensions in)))
	  (let* ((dims-l (or (and (and w h) (list h w))
			    dims)))
	    (assert (<= (* (reduce #'* (butlast dims-l)) 
			   (+ 1 (floor (first (last dims-l)) 2)))
			(array-total-size out-d)))
	    (sb-sys:with-pinned-objects (in-d out-d)
	      (let ((r (%fftwf_plan_dft_r2c_2d (first dims-l)
					       (second dims-l)
					       (sb-sys:vector-sap in-d)
					       (sb-sys:vector-sap out-d)
					       sign flag)))
		(when (cffi:null-pointer-p r)
		  (error "plan_dft_r2c_2d didn't succeed."))
		r)))))))

(defun planf (in &key out w h (flag +estimate+) (sign +forward+))
  "Plan a Fast fourier transform with real input of complex single float. If in and out are given, out-of-place transform. In-place transform is not supported (because it would need padding)."
  (declare (type (array (complex single-float) *) in))
  (unless out
    (error "in-place transform not supported."))
  (let* ((in-d (or (array-displacement in) in))
	 (out-d (or (array-displacement out) out)))
    (let* ((dims (array-dimensions in))
	   (rank (array-rank in)))
      (let* ((dims-l (or (and (and w h) (list h w))
			 dims))
	     (dims-in (make-array rank :element-type '(signed-byte 32)
				  :initial-contents dims-l)))
	(assert (<= (* (reduce #'* (butlast dims-l)) 
		       (+ 1 (floor (first (last dims-l)) 2)))
		    (array-total-size out-d)))
	(sb-sys:with-pinned-objects (in-d out-d dims-in)
	  (let ((r (%fftwf_plan_dft  rank
				     (sb-sys:vector-sap dims-in)
				     (sb-sys:vector-sap in-d)
				     (sb-sys:vector-sap out-d)
				     sign flag)))
	    (when (cffi:null-pointer-p r)
	      (error "plan_dft didn't succeed."))
	    r))))))


(defun make-foreign-complex-array-as-double (dims)
  (multiple-value-bind (ivector iptr) #+ccl (ccl:make-heap-ivector (* 2 (reduce #'* dims))
								   'double-float)
		       #+sbcl (let ((a (make-array (* 2 (reduce #'* dims)) :element-type 'double-float)))
				(values a (sb-ext::vector-sap a)))
		       (let* ((darray (make-array (append dims (list 2))
						  :element-type 'double-float
						  :displaced-to ivector)))
			 ;; the following uses ccl:terminate-when-unreachable. it should be
			 ;; possible to use this in cffi
			 (trivial-garbage:finalize darray (lambda () 
							    (format t "getting rid of object with size ~a~%" (length ivector))
							    #+ccl (ccl:dispose-heap-ivector ivector)))
			 (values darray iptr))))

(defun foreign-complex-array-as-double-p (a)
  (and (= 2 (car (last (array-dimensions a))))
       #+ccl (ccl::%heap-ivector-p (array-displacement a))))


;; thread safety: fftw_execute is the only function that can be called
;; from multiple threads. the planner shouldn't be called from
;; multiple
;; threads. http://www.fftw.org/doc/Thread-safety.html#Thread-safety

(declaim (optimize (debug 3)))
(defun ft (in &key out-arg w h (flag +estimate+) (sign +forward+))
  "Plan and execute an out-of-place Fourier transform of the array
'in'. SBCL allows to call the foreign function without copying the
data but the input array 'in' must be a displaced one-dimensional
array. In CCL, the input array and the output array are allocated on
the foreign heap, i.e. if you don't use ccl:make-heap-ivector to
allocate the arrays, the input and output data must be copied."
  #+sbcl (declare (type (array (complex double-float) *) in))
  (let* ((out1 (or (and out-arg (array-displacement out-arg)) (make-array (array-total-size in) :element-type '(complex double-float))))
	 (out  (make-array (array-dimensions in) :element-type '(complex double-float)
			   :displaced-to out1)))
    #+ccl
    (let* ((in-foreign (if (foreign-complex-array-as-double-p in)
			   in
			   (make-foreign-complex-array-as-double (array-dimensions in))))
	   (in-foreign1 (array-displacement in-foreign))
	   (out-foreign (if (and out-arg (foreign-complex-array-as-double-p out-arg))
			    out-arg
			    (make-foreign-complex-array-as-double (array-dimensions in))))
	   (out-foreign1 (array-displacement out-foreign)))
      (unless (foreign-complex-array-as-double-p in)
	(let ((in1 (make-array (array-total-size in) :element-type '(complex double-float)
			       :displaced-to in)))
	  (dotimes (i (length in1))
	    (setf (aref in-foreign1 (* 2 i)) (realpart (aref in1 i))
		  (aref in-foreign1 (+ 1 (* 2 i))) (imagpart (aref in1 i))))))
      (let ((plan (plan in-foreign out-foreign)))
	(%fftw_execute plan)
	(if out-arg 
	    out-arg
	    (progn
	      (dotimes (i (length out1))
		(setf (aref out1 i) (complex (aref out-foreign1 (* 2 i))
					     (aref out-foreign1 (+ 1 (* 2 i))))))
	      out))))
    #+sbcl
    (if (sb-impl::array-header-p in)
	(if (sb-impl::%array-displaced-p in)
	    (let ((in1 (array-displacement in)))
	      (if (and in1 (equal '(complex double-float) (array-element-type in)))
	       (progn
		 (with-pointer-to-vector-data (in-sap in1)
		   (declare (ignore in-sap)) ;; i just do this in order to pin the array
		   (with-pointer-to-vector-data (out-sap out1)
		     (declare (ignore out-sap))
		     (let ((plan (plan in :out out :w w :h h :flag flag :sign sign)))
		       (%fftw_execute plan))))
		 out)
	       (error "input array is not complex double-float. I can't work with this.")
	       ))
	    (let ((in1 (sb-ext:array-storage-vector in)))
	      (progn
		 (with-pointer-to-vector-data (in-sap in1)
		   (declare (ignore in-sap)) ;; i just do this in order to pin the array
;; the array must be pinned from plan creation to execution
		   (with-pointer-to-vector-data (out-sap out1)
		     (declare (ignore out-sap))
		     (let ((plan (plan in :out out :w w :h h)))
		       (%fftw_execute plan))))
		 out)))
	(error "input array is not displaced to 1d array. I can't work with this."))
    ))

(defun rftf (in &key out-arg w h (flag +estimate+) (sign +forward+))
  "Plan and execute an out-of-place Fourier transform of the real
single-float array 'in'."
  (declare (type (array single-float *) in))
  (let* ((dims-l (or (and (and w h) (list h w))
		    (array-dimensions in)))
	(odims-l (append (butlast dims-l)
			 (+ 1 (floor (first (last dims-l)) 2)))))
    (let* ((out1 (or (and out-arg (array-displacement out-arg))
		     (make-array (reduce #'* odims-l)
				 :element-type '(complex single-float))))
	   (out  (make-array odims-l :element-type '(complex double-float)
			     :displaced-to out1)))
      (sb-sys:with-pinned-objects (out1 in)
	(let (#+nil (in1 (cond
		     ((and (sb-impl::array-header-p in)
			   (sb-impl::%array-displaced-p in))
		      (if (equal 'single-float (array-element-type in))
			  (array-displacement in)
			  (error "input array is not single-float. I can't work with this.")))
		     ((and (sb-impl::array-header-p in)
			   (sb-ext:array-storage-vector in))
		      (sb-ext:array-storage-vector in))
		     (t (error "input array is neither displaced to 1d array nor simple-array. I can't work with this.")))))
	  (let ((plan (rplanf in :out out :w w :h h
			      :flag flag :sign sign)))
	    (%fftwf_execute plan))
	  out)))))

