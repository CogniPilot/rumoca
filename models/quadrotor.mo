model Quadrotor
    constant Real pi;
    parameter Real c;
    Real x; // test
    Real y;
equation
    der(x) = 1.0*pi;
    der(y) = x + 3*y*c;
end Quadrotor;
