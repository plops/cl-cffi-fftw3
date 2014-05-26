(in-package :fftw)

;; wget ftp://ftp.fftw.org/pub/fftw/fftw-3.3.4-dll32.zip

(define-foreign-library fftw3
  (t #+linux (:default "libfftw3")
     #-linux (:default "d:/martin/cl-cffi-fftw3/libfftw3-3")))

(define-foreign-library fftw3_threads
  (t (:default "libfftw3_threads")))

(defun load-fftw-libraries ()
  (use-foreign-library fftw3)
  #+linux (use-foreign-library fftw3_threads))
