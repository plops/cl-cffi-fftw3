#include <fftw3.h>
#include <stdio.h>

int
main()
{
  float in[512*512]; 
  fftwf_complex out[512*512];
  fftwf_init_threads();
  fftwf_plan_with_nthreads(4);
  fftwf_plan plan = fftwf_plan_dft_r2c_2d(512,512,in,out,FFTW_PATIENT);
  printf("0x%lx\n",(unsigned long)plan);
  return 0;
}
