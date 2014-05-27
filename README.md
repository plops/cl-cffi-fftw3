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
parameter n defaults to the number of processors. This function
doesn't have to be called, if only one thread should be used for
calculations.


```
ft in [out]
```

Plan and execute an out-of-place Fourier transform of the array
'in'. The input array 'in' must be a displaced one-dimensional array.
The transform is calculated out-of-place if no output array 'out' is
given.

In Clozure Common Lisp the fastest speed can be obtained when
preallocated arrays (using the function 
make-foreign-complex-array-as-double

``` make-foreign-complex-array-as-double dimension ```

This function only makes sense for Clozure Common Lisp.  It allocates
an array on the foreign stack and registers a finalizer to free the
memory when the reference is no longer used.  dimension is a list of
positive integers, e.g. '(17 23). Note that Clozure Common Lisp
doesn't support (complex double-float) element types as heap-ivector.
Until I find a better solution make-foreign-complex-array-as-double
returns a double-float array with dimensions (17 23 2), i.e. the last
dimension indexes real (0) and imaginary parts (1).

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

# supported implementations:

* SBCL .. works (linux64, win32 8.1)
* CCL  .. works (linux64, win32 8.1)


# todo

* improve performance for CCL

* for best performance fftw3 wants its arrays to be allocated with a particular alignment. i haven't checked if this is the case