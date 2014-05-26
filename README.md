cl-cffi-fftw3 is a foreign function binding to call fftw3 from common
lisp using cffi. i want it to run in sbcl and clozure common lisp. my
application are in particular multi-dimensional complex transforms
with arbitrary array sizes.

i would be happy if there was a way to minimize unnecessary copying of
the arrays. e.g. using sbcl's sb-sys:array-storage-vector. however,
apparently this is quite difficult to get right and cffi doesn't
support it. i put more emphasize on portability and correctness than
speed.
