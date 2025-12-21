// Test Complex type causality propagation
// Mimics MSL's ComplexBlocks pattern
package ComplexCausality
  // Define our own Complex-like record to avoid needing the builtin
  record MyComplex
    Real re "Real part";
    Real im "Imaginary part";
  end MyComplex;

  connector MyComplexInput = input MyComplex;

  connector MyComplexOutput = output MyComplex;

  block CSISO "Complex single input single output"
    MyComplexInput u "Input";
    MyComplexOutput y "Output";
  end CSISO;

  block ComplexGain "Simple Complex gain block"
    extends CSISO;
    parameter Real k = 1 "Gain";
  equation
    y.re = k * u.re;
    y.im = k * u.im;
  end ComplexGain;

  // Array version matching ToSpacePhasor issue
  block ComplexArrayInput "Block with array of Complex inputs"
    parameter Integer m = 3;
    MyComplexInput u[m] "Complex input array";
    output Real y[2] "Output";
  equation
    // Simple equations - sum of real and imaginary parts
    y[1] = sum(u[i].re for i in 1:m);
    y[2] = sum(u[i].im for i in 1:m);
  end ComplexArrayInput;

  // Test protected Complex-typed components with binding (like ComplexSI2SO)
  block CSI2SO "Complex single input 2 single output - mimics MSL ComplexSI2SO"
    MyComplexInput u1 "First input";
    MyComplexInput u2 "Second input";
    MyComplexOutput y "Output";
  protected
    // These have binding expressions that need Complex expansion
    MyComplex u1Internal = u1;
    MyComplex u2Internal = u2;
  end CSI2SO;

  // Test block extending CSI2SO - mimics ComplexMath.Add
  block ComplexAdd "Complex add block"
    extends CSI2SO;
    parameter Real k1 = 1 "Gain for u1";
    parameter Real k2 = 1 "Gain for u2";
  equation
    // This equation uses the protected u1Internal and u2Internal
    y.re = k1 * u1Internal.re + k2 * u2Internal.re;
    y.im = k1 * u1Internal.im + k2 * u2Internal.im;
  end ComplexAdd;

  // Test with conditional binding expressions (like MSL's ComplexSI2SO with useConjugateInput)
  // Using a simpler pattern: just select between two inputs based on a parameter
  block CSI2SO_Conditional "Complex interface with conditional binding"
    MyComplexInput u1 "First input";
    MyComplexInput u2 "Second input";
    MyComplexOutput y "Output";
    parameter Boolean swapInputs = false;
  protected
    // Conditional binding expressions - select between u1 and u2 based on parameter
    // This tests that if-expressions in binding get expanded properly
    MyComplex u1Internal = if swapInputs then u2 else u1;
    MyComplex u2Internal = if swapInputs then u1 else u2;
  end CSI2SO_Conditional;

  // Test block extending conditional interface
  block ComplexAddConditional "Complex add with conditional binding"
    extends CSI2SO_Conditional;
    parameter Real k1 = 1 "Gain for u1";
    parameter Real k2 = 1 "Gain for u2";
  equation
    y.re = k1 * u1Internal.re + k2 * u2Internal.re;
    y.im = k1 * u1Internal.im + k2 * u2Internal.im;
  end ComplexAddConditional;

  // Test using builtin Complex type (like MSL)
  // This matches the MSL pattern exactly: connector aliases and Complex components
  connector BuiltinComplexInput = input Complex;

  connector BuiltinComplexOutput = output Complex;

  block BuiltinComplexAdd "Using builtin Complex type like MSL"
    BuiltinComplexInput u1 "First input";
    BuiltinComplexInput u2 "Second input";
    BuiltinComplexOutput y "Output";
    parameter Real k1 = 1 "Gain for u1";
    parameter Real k2 = 1 "Gain for u2";
  protected
    // Simple binding to test builtin Complex expansion
    Complex u1Internal = u1;
    Complex u2Internal = u2;
  equation
    y.re = k1 * u1Internal.re + k2 * u2Internal.re;
    y.im = k1 * u1Internal.im + k2 * u2Internal.im;
  end BuiltinComplexAdd;

  // Define a conj function like MSL's Modelica.ComplexMath.conj
  // Using exact MSL pattern: c2 := Complex(c1.re, -c1.im);
  function conj "Return conjugate of complex number"
    input Complex c1 "Complex number";
    output Complex c2 "Conjugate";
  algorithm
    c2 := Complex(c1.re, -c1.im);
  end conj;

  // Test block using conj() exactly like MSL's ComplexSI2SO pattern
  block ConjComplexAdd "Full MSL pattern with conj function"
    BuiltinComplexInput u1 "First input";
    BuiltinComplexInput u2 "Second input";
    BuiltinComplexOutput y "Output";
    parameter Boolean useConjugateInput1 = false;
    parameter Boolean useConjugateInput2 = false;
    parameter Real k1 = 1 "Gain for u1";
    parameter Real k2 = 1 "Gain for u2";
  protected
    // Exactly like MSL's ComplexSI2SO: (if cond then conj(u) else u)
    Complex u1Internal = (if useConjugateInput1 then conj(u1) else u1);
    Complex u2Internal = (if useConjugateInput2 then conj(u2) else u2);
  equation
    y.re = k1 * u1Internal.re + k2 * u2Internal.re;
    y.im = k1 * u1Internal.im + k2 * u2Internal.im;
  end ConjComplexAdd;

  // Test with parenthesized conditional binding (exactly like MSL's ComplexSI2SO)
  block ParenthesizedComplexAdd "Matches MSL pattern with parenthesized binding"
    BuiltinComplexInput u1 "First input";
    BuiltinComplexInput u2 "Second input";
    BuiltinComplexOutput y "Output";
    parameter Boolean useConjugateInput1 = false;
    parameter Boolean useConjugateInput2 = false;
    parameter Real k1 = 1 "Gain for u1";
    parameter Real k2 = 1 "Gain for u2";
  protected
    // Exactly like MSL: parenthesized conditional, selecting between u1/u2
    // Note: we can't use conj() here since it's not defined in our test package
    Complex u1Internal = (if useConjugateInput1 then u2 else u1);
    Complex u2Internal = (if useConjugateInput2 then u1 else u2);
  equation
    y.re = k1 * u1Internal.re + k2 * u2Internal.re;
    y.im = k1 * u1Internal.im + k2 * u2Internal.im;
  end ParenthesizedComplexAdd;
end ComplexCausality;
