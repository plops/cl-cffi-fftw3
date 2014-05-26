(declaim (optimize (debug 3) (safety 3) (speed 0)))
#+nil
(setf asdf:*central-registry*
   ;; Default directories, usually just the ``current directory''
  '(*default-pathname-defaults*

    ;; Additional places where ASDF can find
    ;; system definition files
    #p"/home/martin/stage/cl-cffi-fftw3/"))
#+nil
(asdf:load-system "fftw")

(fftw:prepare-threads)

(let* ((n 1024)
       (a1 (make-array (* n n) :element-type '(complex double-float)))
       (a (make-array (list n n) :element-type '(complex double-float)
		      :displaced-to a1)))
  (defparameter *bla* (fftw:ft a)))
