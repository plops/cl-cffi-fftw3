#+nil
(setf asdf:*central-registry*
   ;; Default directories, usually just the ``current directory''
  '(*default-pathname-defaults*

    ;; Additional places where ASDF can find
    ;; system definition files
    #p"/home/martin/stage/cl-cffi-fftw3/"))
#+nil
(asdf:load-system "fftw")
