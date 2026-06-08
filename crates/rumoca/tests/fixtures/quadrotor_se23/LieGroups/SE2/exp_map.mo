within LieGroups.SE2;
function exp_map "Exponential map: se(2) -> SE(2)"
  input Real xi[3] "Lie algebra {vx, vy, omega}";
  output Real X[3] "Group element {px, py, theta}";
protected
  Real omega;
  Real a "sin(omega)/omega";
  Real b "(1-cos(omega))/omega";
  constant Real eps = 1e-8;
algorithm
  omega := xi[3];

  if abs(omega) < eps then
    // Taylor series near omega=0
    a := 1.0 - omega^2 / 6.0;
    b := omega / 2.0 - omega^3 / 24.0;
  else
    a := sin(omega) / omega;
    b := (1.0 - cos(omega)) / omega;
  end if;

  // p = V(omega) * v, where V = {{a, -b}, {b, a}}
  X[1] := a*xi[1] - b*xi[2];
  X[2] := b*xi[1] + a*xi[2];
  X[3] := omega;
end exp_map;
