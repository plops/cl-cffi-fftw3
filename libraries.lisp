(in-package :fftw)

(define-foreign-library fftw3
  (t (:default "libfftw3.so.3")))

(define-foreign-library fftw3_threads
  (t (:default "libfftw3_threads.so.3")))

(defun load-fftw-libraries ()
  (use-foreign-library fftw)
  (use-foreign-library fftw_threads))
