package FixedFramePendulum
  record Frame "Planar orientation: rotation matrix and angular velocity"
    Real T[2, 2];
    Real w;
  end Frame;

  function planarRotation "Frame rotated by a fixed angle"
    input Real angle;
    output Frame R;
  algorithm
    R := Frame(T = {{cos(angle), -sin(angle)}, {sin(angle), cos(angle)}}, w = 0);
  end planarRotation;

  function resolve "Vector resolved in a rotated frame"
    input Frame R;
    input Real v[2];
    output Real r[2];
  algorithm
    r := R.T*v;
  end resolve;

  model Pendulum "Point mass on a rod whose length constraint is written in a fixed rotated frame"
    parameter Real theta = 0.4;
    parameter Real L = 1;
    parameter Real g = 9.81;
    Frame F;
    Real p[2] "Position resolved in the rotated frame";
    Real x(start = 0.6, fixed = true);
    Real y(start = -0.8);
    Real vx(start = 0, fixed = true);
    Real vy;
    Real lambda;
  equation
    F = planarRotation(theta);
    p = resolve(F, {x, y});
    der(x) = vx;
    der(y) = vy;
    der(vx) = -lambda*x;
    der(vy) = -lambda*y - g;
    p*p = L^2;
  end Pendulum;
end FixedFramePendulum;
