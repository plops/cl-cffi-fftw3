(defpackage #:cl-cffi-fftw3
    (:use #:cl #:cffi)
    (:export
     #:make-foreign-complex-array-as-double
     #:prepare-threads
     #:ft))

(declaim (optimize (debug 3) (safety 3) (speed 0)))
