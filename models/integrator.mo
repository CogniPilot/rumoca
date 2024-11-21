model Integrator
    Real x; // test
    Real y;
equation
    der(x) = 1.0;
    der(y) = x + 3*y;
end Integrator;
