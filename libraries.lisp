(in-package :fftw)

(define-foreign-library fftw3
  (t (:default "libfftw3")))

(define-foreign-library fftw3_threads
  (t (:default "libfftw3_threads")))

(defun load-fftw-libraries ()
  (use-foreign-library fftw3)
  (use-foreign-library fftw3_threads))
