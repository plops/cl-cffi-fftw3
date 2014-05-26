(defpackage #:fftw
    (:use #:cl #:cffi)
    (:export
     #:prepare-threads
     #:ft))

(declaim (optimize (debug 3) (safety 3) (speed 0)))
