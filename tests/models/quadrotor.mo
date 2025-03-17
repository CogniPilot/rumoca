model Quadrotor
    Motor m1, m2;
equation
    m1.omega_ref = time;
    m2.omega_ref = time;
end Quadrotor;

model Motor
    parameter Real tau = 1.0;
    input Real omega_ref;
    output Real omega;
equation
    der(omega) = (1/tau) * (omega_ref - omega);
end Motor;