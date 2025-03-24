model Quadrotor
    // stevens and lewis pg 111
    parameter Real m = 1.0;
    parameter Real g = 9.81;
    parameter Real Jx = 1;
    parameter Real Jy = 1;
    parameter Real Jz = 1;
    parameter Real Jxz = 0.0;
    parameter Real m = 1.0;
    parameter Real Lambda = 0.01; // Jx*Jz - Jxz*Jxz;
    parameter Real l = 1.0;
    Real x, y, z;
    Real P, Q, R;
    Real U, V, W;
    Real Fx, Fy, Fz;
    Real Mx, My, Mz;
    Real phi, theta, psi;
    Motor m1, m2, m3, m4;
    input Real u1, u2, u3, u4;
equation
    // force equations
    der(U) = R*V - Q*W - g*sin(theta) + Fx/m;
    der(V) = -R*U + P*W + g*sin(phi)*cos(theta) + Fy/m;
    der(W) = Q*U - P*V + g*cos(phi)*cos(theta) + Fz/m;

    // kinematic equations
    der(phi) = P;
    der(theta) = Q;
    der(psi) = R;

    // moment equations
    Lambda*der(P) = Jxz*(Jx - Jy + Jz)*P*Q - (Jz*(Jz - Jy) + Jxz*Jxz)*Q*R + Jz*Mx + Jxz*Mz;
    Jy*der(Q) = (Jz - Jx)*P*R - Jxz*(P*P - R*R) + My;
    Lambda*der(R) = ((Jx - Jy)*Jx + Jxz*Jxz)*P*Q - Jxz*(Jx - Jy + Jz)*Q*R + Jxz*Mx + Jx*Mz;

    // navigation equations
    der(x) = U*cos(theta)*cos(psi) + V*(-cos(phi)*sin(psi) + sin(phi)*sin(theta)*cos(psi)) + W*(sin(phi)*sin(psi) + cos(phi)*sin(theta)*cos(psi));
    der(y) = U*cos(theta)*sin(psi) + V*(cos(phi)*sin(psi) + sin(phi)*sin(theta)*sin(psi)) + W*(-sin(phi)*cos(psi) + cos(phi)*sin(theta)*sin(psi));
    der(z) = U*sin(theta) - V*sin(phi)*cos(theta) - W*cos(phi)*cos(theta);
    
    // body forces
    Fx = 0;
    Fy = 0;
    Fz = m1.thrust + m2.thrust + m3.thrust + m4.thrust;

    // momments
    Mx = 0*l*(-m1.thrust + m2.thrust - m3.thrust + m4.thrust);
    My = 0*l*(-m1.thrust + m2.thrust + m3.thrust - m4.thrust);
    Mz = m1.moment + m2.moment - m3.moment - m4.moment;

    // motor equations
    m1.omega_ref = u1;
    m2.omega_ref = u2;
    m3.omega_ref = u3;
    m4.omega_ref = u4;
end Quadrotor;

model Motor
    parameter Real Cm = 0;
    parameter Real Ct = 1.0;
    parameter Real tau = 10.0;
    Real omega_ref;
    Real omega;
    Real thrust;
    Real moment;
equation
    der(omega) = (1/tau) * (omega_ref - omega);
    thrust = Ct*omega*omega;
    moment = Cm*thrust;
end Motor;