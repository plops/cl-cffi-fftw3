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
(defcfun (%fftwf_execute "fftwf_execute")
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
(defcfun (%fftwf_plan_dft "fftwf_plan_dft")
    plan-pointer
  (rank :int)
  (n (:pointer :int))
  (in :pointer)
  (out :pointer)
  (sign :int)
  (flags :unsigned-int))

(defcfun (%fftw_plan_dft_r2c "fftw_plan_dft_r2c")
    plan-pointer
  (rank :int)
  (n (:pointer :int))
  (in :pointer)
  (out :pointer)
  (sign :int)
  (flags :unsigned-int))

(defcfun (%fftw_plan_dft_r2c_1d "fftw_plan_dft_1d")
    plan-pointer
  (n :int)
  (in :pointer)
  (out :pointer)
  (sign :int)
  (flags :unsigned-int))

(defcfun (%fftwf_plan_dft_r2c "fftwf_plan_dft_r2c")
    plan-pointer
  (rank :int)
  (n (:pointer :int))
  (in :pointer)
  (out :pointer)
  (sign :int)
  (flags :unsigned-int))

;; functions in the fftw3_threads library

(defcfun (%fftw_init_threads "fftw_init_threads") :int)
(defcfun (%fftwf_init_threads "fftwf_init_threads") :int)
(defcfun (%fftw_plan_with_nthreads "fftw_plan_with_nthreads")
    :void
  (nthreads :int))
(defcfun (%fftwf_plan_with_nthreads "fftwf_plan_with_nthreads")
    :void
  (nthreads :int))



(defcfun (%fftw_export_wisdom_to_filename "fftw_export_wisdom_to_filename")
    :int ;; non-zero on success
  (filename :string))

(defcfun (%fftw_import_wisdom_from_filename "fftw_import_wisdom_from_filename")
    :int
  (filename :string))


(defcfun (%fftwf_export_wisdom_to_filename "fftwf_export_wisdom_to_filename")
    :int ;; non-zero on success
  (filename :string))

(defcfun (%fftwf_import_wisdom_from_filename "fftwf_import_wisdom_from_filename")
    :int
  (filename :string))
   
