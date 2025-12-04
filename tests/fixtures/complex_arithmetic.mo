// Test Complex number arithmetic
record Complex "Complex number"
  Real re "Real part";
  Real im "Imaginary part";
end Complex;

model ComplexAddition "Test Complex addition"
  Complex a(re = 1, im = 2);
  Complex b(re = 3, im = 4);
  Complex c;
equation
  // Field-wise addition should work since records are flattened
  c.re = a.re + b.re;
  c.im = a.im + b.im;
end ComplexAddition;

model ComplexMultiplication "Test Complex multiplication"
  Complex a(re = 1, im = 2);
  Complex b(re = 3, im = 4);
  Complex c;
  // c = a * b
  // c.re = a.re*b.re - a.im*b.im = 1*3 - 2*4 = -5
equation
  // c.im = a.re*b.im + a.im*b.re = 1*4 + 2*3 = 10
  c.re = a.re * b.re - a.im * b.im;
  c.im = a.re * b.im + a.im * b.re;
end ComplexMultiplication;
