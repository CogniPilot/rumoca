model Quadrotor
    parameter Real c;
    Real x; // test
    Real y;
equation
    der(x) = 1.0;
    der(y) = x + 3*y*c;
end Quadrotor;
