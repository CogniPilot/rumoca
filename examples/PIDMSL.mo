model PIDMSL
    import Modelica.Blocks.Continuous.PID;
    Real x;
    PID pid(k=10, Ti=1, Td=0.1);
equation
    pid.u = 1 - x;
    der(x) = x + pid.y;
end PIDMSL;