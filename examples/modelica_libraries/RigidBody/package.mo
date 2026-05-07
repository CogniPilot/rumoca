package RigidBody
  partial model RigidBody6DOF
    parameter Real mass = 1.0 "Mass [kg]";
    parameter Real g = 9.8 "Gravity [m/s^2]";
    parameter Real ixx = 1.0 "Body inertia matrix xx entry [kg*m^2]";
    parameter Real iyy = 1.0 "Body inertia matrix yy entry [kg*m^2]";
    parameter Real izz = 1.0 "Body inertia matrix zz entry [kg*m^2]";
    parameter Real ixy = 0.0 "Body inertia matrix xy entry [kg*m^2]";
    parameter Real ixz = 0.0 "Body inertia matrix xz entry [kg*m^2]";
    parameter Real iyz = 0.0 "Body inertia matrix yz entry [kg*m^2]";
    parameter Real J[3, 3] = [
      ixx, ixy, ixz;
      ixy, iyy, iyz;
      ixz, iyz, izz
    ] "Body inertia matrix [kg*m^2]";
    parameter Real p_start[3] = {0, 0, 0} "Initial world position";
    parameter Real v_b_start[3] = {0, 0, 0} "Initial body velocity";
    parameter Real q_start[4] = {1, 0, 0, 0} "Initial quaternion w,x,y,z";
    parameter Real omega_start[3] = {0, 0, 0} "Initial body angular velocity";
    parameter Real qnorm_gain = 1.0 "Quaternion renormalization gain";

    Real F_b[3] "Total non-gravity force in body frame [N]";
    Real M_b[3] "Total moment in body frame [N*m]";
    output Real p[3](start = p_start, each fixed = true) "World position [m]";
    output Real v_b[3](start = v_b_start, each fixed = true) "Body velocity [m/s]";
    output Real q[4](start = q_start) "Quaternion w,x,y,z";
    output Real omega[3](start = omega_start, each fixed = true) "Body angular velocity [rad/s]";
    output Real R[3, 3](start = [
      1, 0, 0;
      0, 1, 0;
      0, 0, 1
    ]) "Direction cosine matrix, body to world";
    output Real v_w[3](start = v_b_start) "World velocity [m/s]";
    output Real a_b[3](start = {0, 0, 0}) "Body specific force [m/s^2]";

  protected
    LieGroup.SO3.Quaternion attitude(q_start = q_start, qnorm_gain = qnorm_gain);
    Real gravity_w[3] "Gravity in world frame [m/s^2]";
    Real gravity_b[3] "Gravity in body frame [m/s^2]";
    Real H_b[3] "Angular momentum in body frame [kg*m^2/s]";
    Real M_gyro[3] "Gyroscopic inertia moment in body frame [N*m]";
    Real M_body[3] "Rigid-body angular acceleration moment [N*m]";

  equation
    attitude.omega = omega;
    q = attitude.q;
    R = attitude.R;

    v_w = R * v_b;
    gravity_w = {0, 0, -g};
    gravity_b = transpose(R) * gravity_w;
    a_b = F_b / mass;

    der(p) = v_w;
    der(v_b) = a_b + gravity_b - cross(omega, v_b);

    H_b = J * omega;
    M_gyro = cross(omega, H_b);
    M_body = M_b - M_gyro;
    J * der(omega) = M_body;
  end RigidBody6DOF;
end RigidBody;
