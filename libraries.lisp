(in-package :fftw)

;; wget ftp://ftp.fftw.org/pub/fftw/fftw-3.3.4-dll32.zip

(define-foreign-library fftw3
  (t #+linux (:default "libfftw3")
     #-linux (:default "d:/martin/cl-cffi-fftw3/libfftw3-3")))

(define-foreign-library fftw3f
  (t #+linux (:default "libfftw3f")))

(define-foreign-library fftw3_threads
  (t (:default "libfftw3_threads")))

(define-foreign-library fftw3f_threads
  (t (:default "libfftw3f_threads")))

(defun load-fftw-libraries ()
  (use-foreign-library fftw3)
  (use-foreign-library fftw3f)
  #+linux (use-foreign-library fftw3_threads)
  #+linux (use-foreign-library fftw3f_threads))

#+nil
(cffi:list-foreign-libraries)
