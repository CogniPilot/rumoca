model Quadrotor
    extends RigidBody6DOF;
    parameter Real l = 1.0;
    parameter Real mix_a = 1;
    parameter Real mix_e = 1;
    parameter Real mix_r = 10;
    parameter Real mix_t = 32.0;
    Motor m_1, m_2, m_3, m_4;
    input Real a "aileron";
    input Real e "elevator";
    input Real r "rudder";
    input Real t "throttle";
equation
    // body forces
    F_x = -(m*g)*sin(theta);
    F_y = (m*g)*sin(phi)*cos(theta);
    F_z = (m*g)*cos(phi)*cos(theta) -
        (m_1.thrust + m_2.thrust + m_3.thrust + m_4.thrust);

    // body momments
    M_x = l*(-m_1.thrust + m_2.thrust - m_3.thrust + m_4.thrust);
    M_y = l*(-m_1.thrust + m_2.thrust + m_3.thrust - m_4.thrust);
    M_z = m_1.moment + m_2.moment - m_3.moment - m_4.moment;

    // motor equations
    m_1.omega_ref = t*mix_t - a*mix_a + e*mix_e + r*mix_r;
    m_2.omega_ref = t*mix_t + a*mix_a - e*mix_e + r*mix_r;
    m_3.omega_ref = t*mix_t - a*mix_a - e*mix_e - r*mix_r;
    m_4.omega_ref = t*mix_t + a*mix_a + e*mix_e - r*mix_r;
end Quadrotor;

model RigidBody6DOF
    // stevens and lewis pg 111
    parameter Real m = 1.0;
    parameter Real g = 9.81;
    parameter Real J_x = 1;
    parameter Real J_y = 1;
    parameter Real J_z = 1;
    parameter Real J_xz = 0.0;
    parameter Real Lambda = 1; // Jx*Jz - Jxz*Jxz;
    Real x, y, h;
    Real P, Q, R;
    Real U, V, W;
    Real F_x, F_y, F_z;
    Real M_x, M_y, M_z;
    Real phi, theta, psi;
equation
    // navigation equations
    der(x) = U*cos(theta)*cos(psi) + V*(-cos(phi)*sin(psi) + sin(phi)*sin(theta)*cos(psi)) + W*(sin(phi)*sin(psi) + cos(phi)*sin(theta)*cos(psi));
    der(y) = U*cos(theta)*sin(psi) + V*(cos(phi)*cos(psi) + sin(phi)*sin(theta)*sin(psi)) + W*(-sin(phi)*cos(psi) + cos(phi)*sin(theta)*sin(psi));
    der(h) = U*sin(theta) - V*sin(phi)*cos(theta) - W*cos(phi)*cos(theta);

    // force equations
    der(U) = R*V - Q*W + F_x/m;
    der(V) = -R*U + P*W + F_y/m;
    der(W) = Q*U - P*V + F_z/m;

    // kinematic equations
    der(phi) = P + tan(theta)*(Q*sin(phi) + R*cos(phi));
    der(theta) = Q*cos(phi) - R*sin(phi);
    der(psi) = (Q*sin(phi) + R*cos(phi))/cos(theta);

    // moment equations
    der(P) = (J_xz*(J_x - J_y + J_z)*P*Q - (J_z*(J_z - J_y) + J_xz*J_xz)*Q*R + J_z*M_x + J_xz*M_z)/Lambda;
    der(Q) = ((J_z - J_x)*P*R - J_xz*(P*P - R*R) + M_y)/J_y;
    der(R) = (((J_x - J_y)*J_x + J_xz*J_xz)*P*Q - J_xz*(J_x - J_y + J_z)*Q*R + J_xz*M_x + J_x*M_z)/Lambda;


end RigidBody6DOF;

model Motor
    parameter Real Cm = 0.01;
    parameter Real Ct = 0.01;
    parameter Real tau = 0.1;
    Real omega_ref;
    Real omega;
    Real thrust;
    Real moment;
equation
    der(omega) = (1/tau) * (omega_ref - omega);
    thrust = Ct*omega*omega;
    moment = Cm*thrust;
end Motor;