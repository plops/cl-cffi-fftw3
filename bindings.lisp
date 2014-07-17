(in-package :fftw)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load-fftw-libraries))

;; constants

(defconstant +forward+ -1)
(defconstant +backward+ 1)
(defconstant +measure+ 0)
(defconstant +estimate+ (ash 1 6)) ;; array isn't overwritten during planning
(defconstant +patient+ (ash 1 5)) 

;; types

(defctype plan-pointer :pointer "Pointer to FFTW plan definition")


;; function definitions

(defcfun (%fftw_execute "fftw_execute")
    :void (plan plan-pointer))
(defcfun (%sfftw_execute "sfftw_execute")
    :void (plan plan-pointer))
(defcfun (%sftw_destroy_plan "fftw_destroy_plan")
    :void (plan plan-pointer))


(defcfun (%fftw_plan_dft "fftw_plan_dft")
    plan-pointer
  (rank :int)
  (n (:pointer :int))
  (in :pointer) ;; complex double-float
  (out :pointer) ;; complex double-float
  (sign :int)
  (flags :unsigned-int))
(defcfun (%sfftw_plan_dft "sfftw_plan_dft")
    plan-pointer
  (rank :int)
  (n (:pointer :int))
  (in :pointer)
  (out :pointer)
  (sign :int)
  (flags :unsigned-int))

(defcfun (%sfftw_plan_dft_r2c "sfftw_plan_dft_r2c")
    plan-pointer
  (rank :int)
  (n (:pointer :int))
  (in :pointer)
  (out :pointer)
  (sign :int)
  (flags :unsigned-int))

;; functions in the fftw3_threads library

(defcfun (%fftw_init_threads "fftw_init_threads") :int)
(defcfun (%sfftw_init_threads "sfftw_init_threads") :int)
(defcfun (%fftw_plan_with_nthreads "fftw_plan_with_nthreads")
    :void
  (nthreads :int))
(defcfun (%sfftw_plan_with_nthreads "sfftw_plan_with_nthreads")
    :void
  (nthreads :int))


