within MyLib;

package Functions "Utility functions"
  function double
    input Real x;
    output Real y;
  algorithm
    y := 2.0 * x;
  end double;

  function triple
    input Real x;
    output Real y;
  algorithm
    y := 3.0 * x;
  end triple;
end Functions;
