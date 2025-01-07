model Integrator "hello"
    Real y;
    Real x; // test
equation
    der(y) = 1.0;
    der(x) = x + 3*y;
end Integrator;
