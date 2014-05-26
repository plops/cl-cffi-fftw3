# cl-cffi-fftw3

## description

cl-cffi-fftw3 is a foreign function binding to call fftw3 from common
lisp using cffi. i want it to run in sbcl and clozure common lisp. my
application are in particular multi-dimensional complex transforms
with arbitrary array sizes.

## usage:

```
prepare-threads [n]
```

Initialize fftw3_threads to use n threads. On Linux the optional
parameter n defaults to the number of processors.


```
ft a
```

Plan and execute an out-of-place Fourier transform of the array
'in'. The input array 'in' must be a displaced one-dimensional array.
The transform is calculated out-of-place and the result is returned in
a newly allocated array.



## example:


```common-lisp
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
```

output:
```
0000000000000000000000000000000000000
0000000000000000000000000000000000000
0000000000000000000000000000000000000
0000000000000000000000000000000000000
0000400000000000000000000000000000000
0000000000000000000000000000000000000
0000000000000000000000000000000000000
0000000000000000000000000000000000000
0000000000000000000000000000000004000
0000000000000000000000000000000000000
0000000000000000000000000000000000000
0000000000000000000000000000000000000
```