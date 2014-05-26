(in-package :fftw)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-fftw-libraries))

;; constants

(defconstant +forward+ -1)
(defconstant +backward+ 1)
(defconstant +measure+ 0)
(defconstant +estimate+ (ash 1 6)) ;; array isn't overwritten during planning

;; types

(defctype plan-pointer :pointer "Pointer to FFTW plan definition")


;; function definitions

(defcfun (%fftw_execute "fftw_execute")
    :void (plan plan-pointer))
(defcfun (%fftw_destroy_plan "fftw_destroy_plan")
    :void (plan plan-pointer))

#+nil
(defcfun (%fftw_plan_dft "fftw_plan_dft")
    plan-pointer (rank ))


;; (define-alien-routine fftw_plan_dft
;;     plan
;;   (rank int)
;;   (n (* int))
;;   (in (* double-float))	 ;; actually complex
;;   (out (* double-float))	  ;; actually complex
;;   (sign int)
;;   (flags unsigned-int))

;; functions in the fftw3_threads library
  
;; (define-alien-routine ("fftw_init_threads" init-threads)
;;     int)

;; (define-alien-routine ("fftw_plan_with_nthreads" plan-with-nthreads)
;;     void
;;   (nthreads int))


;; (progn
;;  (init-threads)
;;  (plan-with-nthreads 4))

