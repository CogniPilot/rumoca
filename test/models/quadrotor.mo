model Quadrotor "quadrotor model"

    // input
    input Real omega_motor_cmd[4] = {1, 1, 1, 1};

    // states
    output Real position_op_w[3];
    output Real velocity_w_p_b[3];
    output Real quaternion_wb[4];
    output Real omega_wb_b[3];
    output Real omega_motor[4];

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
    parameter Real m = 2.0;
    parameter Real Jx = 0.02167;
    parameter Real Jy = 0.02167;
    parameter Real Jz = 0.02167;

    // local variables
    Real P;
    Real Q;
    Real R;
    Real F_b[3];
    Real tau_inv[4];
    Real thrust[4];

equation
    // local variables
    P = omega_wb_b[1];
    Q = omega_wb_b[2];
    R = omega_wb_b[3];

    F_b[1] = 0;
    F_b[2] = 0;
    F_b[3] = thrust[1] + thrust[2] + thrust[3] + thrust[4];
    thrust = {
        CT * omega_motor[1]^2,
        CT * omega_motor[2]^2,
        CT * omega_motor[3]^2,
        CT * omega_motor[4]^2};
    
    // if (omega_motor_cmd[1] > omega_motor[1]) then
    //   tau_inv[1] = 1.0/tau_up;
    // else
    //   tau_inv[1] = 1.0/tau_down;
    // end if;

    // if (omega_motor_cmd[2] > omega_motor[2]) then
    //   tau_inv[2] = 1.0/tau_up;
    // else
    //   tau_inv[2] = 1.0/tau_down;
    // end if;
    
    // if (omega_motor_cmd[3] > omega_motor[3]) then
    //   tau_inv[3] = 1.0/tau_up;
    // else
    //   tau_inv[3] = 1.0/tau_down;
    // end if;

    // if (omega_motor_cmd[4] > omega_motor[4]) then
    //   tau_inv[4] = 1.0/tau_up;
    // else
    //   tau_inv[4] = 1.0/tau_down;
    // end if;
    
    // state derivative
    der(position_op_w) = {0, 0, 0}; //QuatToMatrix(quaternion_wb) * velocity_w_p_b;
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
