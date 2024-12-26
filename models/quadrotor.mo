model Quadrotor "quadrotor model"
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

    // states
    Real position_op_w[3] = {0, 0, 0};
    Real velocity_w_p_b[3] = {0, 0, 0};
    Real quaternion_wb[4] = {1.0, 0, 0, 0};
    Real omega_wb_b[3] = {0, 0, 0};
    Real omega_motor[4] = {0, 0, 0, 0};

    // input
    input Real omega_motor_cmd[4] = 0;

    // internal variables
    Real CD;
    Real P;
    Real Q;
    Real R;
    Real F_b[3];
    Real qbar;
    Real tau_inv[3];
    Real thrust[3];

equation

    // internal variables
    CD = CD0;
    P = omega_wb_b_0;
    Q = omega_wb_b_1;
    R = omega_wb_b_2;
    Cl = Cl_p * P;
    Cm = Cm_q * Q;
    Cn = -Cn_r * R;
    qbar = 0.5 * rho * V^2;

    // aerodynamics
    F_b_0 = -CD* qbar * S;
    
    // thrust
    thrust = {
        CT * omega_motor[1]^2,
        CT * omega_motor[2]^2,
        CT * omega_motor[3]^2,
        CT * omega_motor[4]^2};
    
    // state derivative
    der(position_op_w) = {0, 0, 0};
    der(velocity_w_p_b) = {0, 0, 0};
    der(quaternion_wb) = {0, 0, 0, 0};
    der(omega_wb_b) = {0, 0, 0};
    der(omega_motor) = {
        tau_inv[1] * (omega_motor_cmd[1] - omega_motor[1]),
        tau_inv[2] * (omega_motor_cmd[2] - omega_motor[2]),
        tau_inv[3] * (omega_motor_cmd[3] - omega_motor[3]),
        tau_inv[4] * (omega_motor_cmd[4] - omega_motor[4])};
end Quadrotor;
