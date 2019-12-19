#include <stdio.h>

typedef struct complex_t {
  float real;
  float imag;
} complex_t;

complex_t add(complex_t x, complex_t y){
  complex_t out;
  out.real = x.real + y.real;
  out.imag = x.imag + y.imag;
  return out;
}

complex_t mul(complex_t x, complex_t y){
  complex_t out;
  out.real = x.real * y.real - x.imag * y.imag;
  out.imag = x.real * y.imag + x.imag * y.real;
  return out;
}

