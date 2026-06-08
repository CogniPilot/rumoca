within LieGroups.SO3.Quat;
function adjoint "Adjoint Ad_X: for SO(3), Ad_q = R (the rotation matrix)"
  input Real q[4] "Unit quaternion {w,x,y,z}";
  output Real Ad[3,3] "3x3 adjoint matrix (equals the DCM)";
algorithm
  Ad := LieGroups.SO3.Quat.to_DCM(q);
  annotation(Documentation(info="<html>
    <p>For SO(3), the adjoint representation is simply the rotation matrix:
    Ad_R(v) = R*v. This rotates Lie algebra elements (angular velocities)
    from one frame to another.</p>
    <p>Used in IEKF error propagation: A_k = Ad_{Exp(-xi*dt)}</p>
  </html>"));
end adjoint;
