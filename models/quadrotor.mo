model Quadrotor "quadrotor model"

    // input
    input Real omega_motor_cmd[4] = 0;

    // states
    output Real position_op_w[3] = {0, 0, 0};
    output Real velocity_w_p_b[3] = {0, 0, 0};
    output Real quaternion_wb[4] = {1.0, 0, 0, 0};
    output Real omega_wb_b[3] = {0, 0, 0};
    output Real omega_motor[4] = {0, 0, 0, 0};

protected

    // constants
    constant Real pi = 3.14;
    constant Real g0 = 9.8;
    constant Real deg2rad = pi/180;

    // parameters
    parameter Real tau_up = 0.0125;
    parameter Real tau_down = 0.025;
    parameter Real dir_motor[4] = {1, 1, -1, -1};
    parameter Real l_motor = 0.25;
    parameter Real theta_motor[4] = {-pi/ 4, 3 * pi/ 4, pi/4, -3  * pi/ 4};
    parameter Real CT = 8.5485e-6;
    parameter Real CM = 0.016;
    parameter Real Cl_p = 0.0;
    parameter Real Cm_q = 0.0;
    parameter Real Cn_r = 0.0;
    parameter Real CD0 = 0.0;
    parameter Real S = 1e-1;
    parameter Real rho = 1.225;
    parameter Real m = 2.0;
    parameter Real Jx = 0.02167;
    parameter Real Jy = 0.02167;
    parameter Real Jz = 0.02167;
    parameter Real noise_power_sqrt_a = 70e-6 * g0;
    parameter Real noise_power_sqrt_omega = 2.8e-3 * deg2rad;
    parameter Real noise_power_sqrt_mag_ = 0;
    parameter Real noise_power_sqrt_gps_pos = 0;

    // local variables
    Real CD;
    Real P;
    Real Q;
    Real R;
    Real F_b[3];
    Real qbar;
    Real tau_inv[3];
    Real thrust[3];

equation
    // local variables
    CD = CD0;
    P = omega_wb_b[1];
    Q = omega_wb_b[2];
    R = omega_wb_b[3];
    Cl = Cl_p * P;
    Cm = Cm_q * Q;
    Cn = -Cn_r * R;
    qbar = 0.5 * rho * V^2;
    F_b_0 = -CD* qbar * S;
    thrust = {
        CT * omega_motor[1]^2,
        CT * omega_motor[2]^2,
        CT * omega_motor[3]^2,
        CT * omega_motor[4]^2};
    
    // state derivative
    der(position_op_w) = QuatToMatrix(quaternion_wb) * velocity_w_p_b;
    der(velocity_w_p_b) = {0, 0, 0};
    der(quaternion_wb) = QuatKinematics(quaternion_wb, omega_wb_b);
    der(omega_wb_b) = {0, 0, 0};
    der(omega_motor) = {
        tau_inv[1] * (omega_motor_cmd[1] - omega_motor[1]),
        tau_inv[2] * (omega_motor_cmd[2] - omega_motor[2]),
        tau_inv[3] * (omega_motor_cmd[3] - omega_motor[3]),
        tau_inv[4] * (omega_motor_cmd[4] - omega_motor[4])};
end Quadrotor;

function QuatProduct
    input Real q[4];
    input Real p[4];
    output Real res[4];
algorithm
    res[1] := q[1] * p[1] - q[2] * p[2] - q[3] * p[3] - q[4] * p[4];
    res[2] := q[2] * p[1] + q[1] * p[2] - q[4] * p[3] + q[3] * p[4];
    res[3] := q[3] * p[1] + q[4] * p[2] + q[1] * p[3] - q[2] * p[4];
    res[4] := q[4] * p[1] - q[3] * p[2] + q[2] * p[3] + q[1] * p[4];
end QuatProduct;

function QuatToMatrix
    input Real q[4];
    output Real R[3, 3];
protected
    Real a;
    Real b;
    Real c;
    Real d;
    Real aa;
    Real bb;
    Real cc;
    Real dd;
algorithm
    a := q[1];
    b := q[2];
    c := q[3];
    d := q[4];
    aa := a * a;
    bb := b * b;
    cc := c * c;
    dd := d * d;
    R[1, 1] := aa + bb - cc - dd;
    R[1, 2] := 2 * (b*c - a*d);
    R[1, 3] := 2 * (b*d + a*c);
    R[2, 1] := 2 * (b*c + a*d);
    R[2, 2] := aa + cc - bb - dd;
    R[2, 3] := 2 * (c*d - a*b);
    R[3, 1] := 2 * (b*d - a*c);
    R[3, 2] := 2 * (c*d + a*b);
    R[3, 3] := aa + dd - bb - cc;
end QuatToMatrix;

function QuatKinematics
    input Real q[4];
    input Real w[3];
    output Real qdot[4];
algorithm
    qdot := QuatProduct(q, cat(1, {0}, w)) / 2;
end QuatKinematics;
