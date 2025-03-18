model Quadrotor
    Motor m1, m2, m3, m4;
    input Real u;
equation
    m1.omega_ref = u;
    m2.omega_ref = u;
    m3.omega_ref = u;
    m4.omega_ref = u;
end Quadrotor;

model Motor
    parameter Real tau = 1.0;
    Real omega_ref;
    Real omega;
equation
    der(omega) = (1/tau) * (omega_ref - omega);
end Motor;