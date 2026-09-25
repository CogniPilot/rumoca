model NoDerivativeRateLoop
  "Index-3 angle closure written through a rotation whose supplied derivative reads the rate field"
  record Rot
    Real phi "Angle";
    Real w "Angular rate";
  end Rot;
  function rotate
    input Rot R;
    input Real v[2];
    output Real r[2];
  algorithm
    r := {cos(R.phi)*v[1] - sin(R.phi)*v[2], sin(R.phi)*v[1] + cos(R.phi)*v[2]};
    annotation(derivative(noDerivative=R) = rotate_der);
  end rotate;
  function rotate_der
    input Rot R;
    input Real v[2];
    input Real v_der[2];
    output Real r_der[2];
  algorithm
    r_der := rotate(R, v_der + {-R.w*v[2], R.w*v[1]});
  end rotate_der;
  parameter Real J1 = 1;
  parameter Real J2 = 2;
  Real q(start = 0.3, fixed = true);
  Real w1(start = 0, fixed = true);
  Real phi2;
  Real w2;
  Real lambda;
  Rot R1;
  Rot R2;
equation
  der(q) = w1;
  der(phi2) = w2;
  J1*der(w1) = -q + lambda*cos(q);
  J2*der(w2) = -lambda*cos(phi2);
  R1.phi = q;
  R1.w = w1;
  R2.phi = phi2;
  R2.w = w2;
  rotate(R2, {1, 0})[2] = rotate(R1, {1, 0})[2];
end NoDerivativeRateLoop;
