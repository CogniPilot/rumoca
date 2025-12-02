model NightVapor
  extends RigidBody6DOF;
  parameter Real g = 9.81;
  parameter Real rho = 1.225;
  parameter Real CL0 = 0.1;
  parameter Real CLa = 3.14;
  parameter Real CD0 = 0.02;
  parameter Real k = 0.02;
  parameter Real S = 0.5;
  input Real a "aileron";
  input Real e "elevator";
  input Real r "rudder";
  input Real t "throttle";
  Real aoa "angle of attack";
  Real q "dynamic pressure";
  Real CL "lift coefficient";
  Real CD "drag coefficient";
  Real L "lift";
  //Real D "drag";
equation
  // aerodynamic
  aoa = 0;
  q = rho * (U * U + V * V + W * W) / 2;
  CL = CL0 + CLa * aoa;
  CD = CD0 + k * CL;
  L = CL * q;
  //D = CD*q*S;
  // body forces
  F_x = t - m * g * sin(theta);
  F_y = m * g * sin(phi) * cos(theta);
  F_z = m * g * cos(phi) * cos(theta);
  // body momments
  M_x = a;
  M_y = e;
  M_z = r;
end NightVapor;

model RigidBody6DOF
  // stevens and lewis pg 111
  parameter Real m = 1.0;
  parameter Real J_x = 1;
  parameter Real J_y = 1;
  parameter Real J_z = 1;
  parameter Real J_xz = 0.0;
  parameter Real Lambda = 1; // Jx * Jz - Jxz * Jxz;
  Real x, y, h;
  Real P, Q, R;
  Real U, V, W;
  Real F_x, F_y, F_z;
  Real M_x, M_y, M_z;
  Real phi, theta, psi;
equation
  // navigation equations
  der(x) = U * cos(theta) * cos(psi) + V * (-cos(phi) * sin(psi) + sin(phi) * sin(theta) * cos(psi)) + W * (sin(phi) * sin(psi) + cos(phi) * sin(theta) * cos(psi));
  der(y) = U * cos(theta) * sin(psi) + V * (cos(phi) * cos(psi) + sin(phi) * sin(theta) * sin(psi)) + W * (-sin(phi) * cos(psi) + cos(phi) * sin(theta) * sin(psi));
  der(h) = U * sin(theta) - V * sin(phi) * cos(theta) - W * cos(phi) * cos(theta);
  // force equations
  der(U) = R * V - Q * W + F_x / m;
  der(V) = -R * U + P * W + F_y / m;
  der(W) = Q * U - P * V + F_z / m;
  // kinematic equations
  der(phi) = P + tan(theta) * (Q * sin(phi) + R * cos(phi));
  der(theta) = Q * cos(phi) - R * sin(phi);
  der(psi) = (Q * sin(phi) + R * cos(phi)) / cos(theta);
  // moment equations
  Lambda * der(P) = J_xz * (J_x - J_y + J_z) * P * Q - (J_z * (J_z - J_y) + J_xz * J_xz) * Q * R + J_z * M_x + J_xz * M_z;
  J_y * der(Q) = (J_z - J_x) * P * R - J_xz * (P * P - R * R) + M_y;
  Lambda * der(R) = ((J_x - J_y) * J_x + J_xz * J_xz) * P * Q - J_xz * (J_x - J_y + J_z) * Q * R + J_xz * M_x + J_x * M_z;
end RigidBody6DOF;
