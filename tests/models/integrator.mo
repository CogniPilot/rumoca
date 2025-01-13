model Integrator "hello"
    Real x;
    Sub c;
equation
    der(x) = c.y;
end Integrator;

model Sub
    output Real y;
equation
    y = cos(t);
end Sub;
