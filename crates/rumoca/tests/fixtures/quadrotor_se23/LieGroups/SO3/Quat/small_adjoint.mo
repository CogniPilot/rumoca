within LieGroups.SO3.Quat;
function small_adjoint "Small adjoint ad_xi: the 3x3 skew-symmetric matrix [v]x"
  input Real v[3] "Lie algebra element (rotation vector)";
  output Real ad[3,3] "3x3 skew-symmetric matrix";
algorithm
  ad[1,1] :=  0;     ad[1,2] := -v[3];  ad[1,3] :=  v[2];
  ad[2,1] :=  v[3];  ad[2,2] :=  0;     ad[2,3] := -v[1];
  ad[3,1] := -v[2];  ad[3,2] :=  v[1];  ad[3,3] :=  0;
  annotation(Documentation(info="<html>
    <p>The small adjoint ad_xi for so(3) is the skew-symmetric (hat/wedge) matrix:
    ad_v(w) = v x w (cross product). [xi_1, xi_2] = ad_{xi_1} xi_2.</p>
    <p>Used in continuous-time IEKF: A_c = -ad_{xi_hat}</p>
  </html>"));
end small_adjoint;
