model Rover
    Motor m1;
    Real x, y, theta, v;
    input Real thr, str;
    discrete Real a;
    parameter Real l = 1.0 "length of chassis";
    parameter Real r = 0.1 "radius of wheel";
equation
    v = r*m1.omega;
    der(x) = v*cos(theta);
    der(y) = v*sin(theta);
    der(theta) = (v/l)*tan(str); 
    m1.omega_ref = thr;
    a = 1;
end Rover;

model Motor
    parameter Real tau = 1.0;
    Real omega_ref;
    Real omega;
equation
    der(omega) = (1/tau) * (omega_ref - omega);
end Motor;