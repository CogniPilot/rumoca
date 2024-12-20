model Quadrotor
    constant Real pi = 3.14;
    parameter Real c = 1.0;
    input Real u;
    Real x; // test
    Real y;
equation
    der(x) = 1.0 + u;
    der(y) = x + 3*y*c;
end Quadrotor;
