package LieGroup
  package Vector
    function norm
      input Real v[:] "Vector";
      input Real eps = 0.0 "Regularization added under the square root";
      output Real n "Euclidean norm";

    algorithm
      n := sqrt(v * v + eps);
    end norm;
  end Vector;

  package SO3
    function quaternionNormError
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real err "Quaternion norm error";

    algorithm
      err := q0 * q0 + q1 * q1 + q2 * q2 + q3 * q3 - 1;
    end quaternionNormError;

    function quaternionDerivativeW
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      input Real wx "Body x angular velocity [rad/s]";
      input Real wy "Body y angular velocity [rad/s]";
      input Real wz "Body z angular velocity [rad/s]";
      input Real qnorm_gain = 1.0 "Quaternion renormalization gain";
      output Real q_dot "Quaternion w derivative";

    algorithm
      q_dot := 0.5 * (-q1 * wx - q2 * wy - q3 * wz) - qnorm_gain * quaternionNormError(q0, q1, q2, q3) * q0;
    end quaternionDerivativeW;

    function quaternionDerivativeX
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      input Real wx "Body x angular velocity [rad/s]";
      input Real wy "Body y angular velocity [rad/s]";
      input Real wz "Body z angular velocity [rad/s]";
      input Real qnorm_gain = 1.0 "Quaternion renormalization gain";
      output Real q_dot "Quaternion x derivative";

    algorithm
      q_dot := 0.5 * (q0 * wx - q3 * wy + q2 * wz) - qnorm_gain * quaternionNormError(q0, q1, q2, q3) * q1;
    end quaternionDerivativeX;

    function quaternionDerivativeY
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      input Real wx "Body x angular velocity [rad/s]";
      input Real wy "Body y angular velocity [rad/s]";
      input Real wz "Body z angular velocity [rad/s]";
      input Real qnorm_gain = 1.0 "Quaternion renormalization gain";
      output Real q_dot "Quaternion y derivative";

    algorithm
      q_dot := 0.5 * (q3 * wx + q0 * wy - q1 * wz) - qnorm_gain * quaternionNormError(q0, q1, q2, q3) * q2;
    end quaternionDerivativeY;

    function quaternionDerivativeZ
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      input Real wx "Body x angular velocity [rad/s]";
      input Real wy "Body y angular velocity [rad/s]";
      input Real wz "Body z angular velocity [rad/s]";
      input Real qnorm_gain = 1.0 "Quaternion renormalization gain";
      output Real q_dot "Quaternion z derivative";

    algorithm
      q_dot := 0.5 * (-q2 * wx + q1 * wy + q0 * wz) - qnorm_gain * quaternionNormError(q0, q1, q2, q3) * q3;
    end quaternionDerivativeZ;

    function rotation11
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 1 - 2 * (q2 * q2 + q3 * q3);
    end rotation11;

    function rotation12
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 2 * (q1 * q2 - q0 * q3);
    end rotation12;

    function rotation13
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 2 * (q1 * q3 + q0 * q2);
    end rotation13;

    function rotation21
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 2 * (q1 * q2 + q0 * q3);
    end rotation21;

    function rotation22
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 1 - 2 * (q1 * q1 + q3 * q3);
    end rotation22;

    function rotation23
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 2 * (q2 * q3 - q0 * q1);
    end rotation23;

    function rotation31
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 2 * (q1 * q3 - q0 * q2);
    end rotation31;

    function rotation32
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 2 * (q2 * q3 + q0 * q1);
    end rotation32;

    function rotation33
      input Real q0 "Quaternion w";
      input Real q1 "Quaternion x";
      input Real q2 "Quaternion y";
      input Real q3 "Quaternion z";
      output Real r;

    algorithm
      r := 1 - 2 * (q1 * q1 + q2 * q2);
    end rotation33;

    model Quaternion
      parameter Real q_start[4] = {1, 0, 0, 0} "Initial quaternion w,x,y,z";
      parameter Real qnorm_gain = 1.0 "Quaternion renormalization gain";

      input Real omega[3] "Body angular velocity [rad/s]";
      output Real q[4](start = q_start, each fixed = true) "Quaternion w,x,y,z";
      output Real R[3, 3](start = [
        1, 0, 0;
        0, 1, 0;
        0, 0, 1
      ]) "Direction cosine matrix, body to world";
      output Real q_norm_err(start = 0) "Quaternion norm error";

    equation
      // MLS §3.7.4 and §10.6: der() vectorizes over arrays; array equations
      // keep the SO(3) kinematics in vector/matrix form.
      q_norm_err = quaternionNormError(q[1], q[2], q[3], q[4]);
      der(q) = {
        quaternionDerivativeW(q[1], q[2], q[3], q[4], omega[1], omega[2], omega[3], qnorm_gain),
        quaternionDerivativeX(q[1], q[2], q[3], q[4], omega[1], omega[2], omega[3], qnorm_gain),
        quaternionDerivativeY(q[1], q[2], q[3], q[4], omega[1], omega[2], omega[3], qnorm_gain),
        quaternionDerivativeZ(q[1], q[2], q[3], q[4], omega[1], omega[2], omega[3], qnorm_gain)
      };
      R = [
        rotation11(q[1], q[2], q[3], q[4]), rotation12(q[1], q[2], q[3], q[4]), rotation13(q[1], q[2], q[3], q[4]);
        rotation21(q[1], q[2], q[3], q[4]), rotation22(q[1], q[2], q[3], q[4]), rotation23(q[1], q[2], q[3], q[4]);
        rotation31(q[1], q[2], q[3], q[4]), rotation32(q[1], q[2], q[3], q[4]), rotation33(q[1], q[2], q[3], q[4])
      ];
    end Quaternion;
  end SO3;
end LieGroup;
