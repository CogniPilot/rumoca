within;

package MathLib
  function add
    input Real a;
    input Real b;
    output Real c;
  algorithm
    c := a + b;
  end add;

  function multiply
    input Real a;
    input Real b;
    output Real c;
  algorithm
    c := a * b;
  end multiply;

  function subtract
    input Real a;
    input Real b;
    output Real c;
  algorithm
    c := a - b;
  end subtract;
end MathLib;

package Utils
  function scale
    input Real k;
    input Real x;
    output Real y;
  algorithm
    y := k * x;
  end scale;
end Utils;

// Test qualified import: import A.B.C;
model QualifiedImportTest
  import MathLib.add;
  Real x = 1;
  Real y = 2;
  Real z;
equation
  z = add(x, y);
end QualifiedImportTest;

// Test renamed import: import D = A.B.C;
model RenamedImportTest
  import plus = MathLib.add;
  Real x = 1;
  Real y = 2;
  Real z;
equation
  z = plus(x, y);
end RenamedImportTest;

// Test unqualified import: import A.B.*;
model UnqualifiedImportTest
  import MathLib.*;
  Real a = 1;
  Real b = 2;
  Real sum_val;
  Real prod_val;
equation
  sum_val = add(a, b);
  prod_val = multiply(a, b);
end UnqualifiedImportTest;

// Test selective import: import A.B.{C, D};
model SelectiveImportTest
  import MathLib.{add, subtract};
  Real x = 5;
  Real y = 3;
  Real sum_val;
  Real diff_val;
equation
  sum_val = add(x, y);
  diff_val = subtract(x, y);
end SelectiveImportTest;

// Test multiple imports in one model
model MultiImportTest
  import MathLib.add;
  import Utils.scale;
  Real x = 2;
  Real y = 3;
  Real sum_val;
  Real scaled;
equation
  sum_val = add(x, y);
  scaled = scale(2, sum_val);
end MultiImportTest;
