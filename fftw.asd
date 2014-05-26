;; i copied the general layout from cffi-redland

(asdf:defsystem fftw
  :version "0"
  :description "CFFI bindings for fftw3 \"Fastest Fourier Transform in the West\"."
  :maintainer "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :author "Martin Kielhorn <kielhorn.martin@gmail.com>"
  :licence "GPL"
  :depends-on (:cffi)
  :components ((:file "package")
               (:file "libraries" :depends-on ("package"))
               (:file "bindings" :depends-on ("package" "libraries"))
               (:file "wrapper" :depends-on ("package" "bindings"))))
