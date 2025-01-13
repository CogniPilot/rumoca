block FlatEarth6DOF
    // input
    input Real Fx, Fy, Fz, Mx, My, Mz;

    // states
    //output Real U, V, W;
    //output Real phi, theta, psi;
    //output Real P, Q, R;
    //output Real pN, pE, h;

    // parameters
    //parameter Real Jx=1, Jy=1, Jxz=0;
    //parameter Real m=1, g=9.8;
    //Real gamma = Jx*Jz - Jxz^2;

equation

    // velocity kinematics
    //der(U) = R*V - Q*W - gD*sin(theta) + Fx/m;
    //der(V) = -R*U + P*W + gD*sin(phi)*cos(theta) + Fy/m;
    //der(W) = Q*U - P*V + gD*cos(phi)*cos(theta) + Fz/m;

    // attitude kinematics
    //der(phi) = P + tan(theta)*(Q*sin(phi) + R*cos(phi));
    //der(theta) = Q*cos(phi) - R*sin(phi);
    //der(psi) = (Q*sin(phi) + R*cos(phi))/cos(phi);

    // attitude dynamics
    //der(P) = (Jxz * (Jx - Jy - Jz)*P*Q - (Jz*(Jz - Jy) + Jxz^2)*Q*R + Jz*Mx + Jxz*Mz) / gamma;
    //der(Q) = ((Jz - Jx)*P*R - Jxz*(P^2 - R^2) + My) / Jy;
    //der(R) = (((Jx - Jy)*Jx + Jxz^2)*P*Q - Jxz*(Jx - Jy + Jz)*Q*R + Jxz*Mx + Jx*Mz) / gamma;

    // navigation equations
    //der(pN) = U*cos(theta)*cos(psi) + V(-cos(phi)*sin(psi) + sin(phi)*sin(theta)*cos(psi)) + W*(sin(phi)*sin(psi) + cos(phi)*sin(theta)*cos(psi));
    //der(pE) = U*cos(theta)*sin(psi) + V*(cos(phi)*cos(psi) + sin(phi)*sin(theta)*sin(psi)) + W*(-sin(phi)*cos(psi) + cos(phi)*sin(theta)*sin(psi));
    //der(h)  = U*sin(theta)          - V*sin(phi)*cos(theta)                                - W*cos(phi)*cos(theta);

end FlatEarth6DOF;