within LieGroups.SO3.Quat;
function exp_map "Exponential map: so(3) rotation vector -> unit quaternion"
  input Real v[3] "Rotation vector (axis * angle)";
  output Real q[4] "Unit quaternion {w,x,y,z}";
protected
  Real theta_sq;
  Real half_theta;
  Real A "sin(theta/2) / theta";
  Real B "cos(theta/2)";
  constant Real eps = 1e-8;
algorithm
  theta_sq := v[1]^2 + v[2]^2 + v[3]^2;
  if theta_sq < eps then
    // Taylor series: cos(t/2) ~ 1 - t^2/8, sin(t/2)/t ~ 1/2 - t^2/48
    B := 1.0 - theta_sq / 8.0;
    A := 0.5 - theta_sq / 48.0;
  else
    half_theta := sqrt(theta_sq) / 2.0;
    B := cos(half_theta);
    A := sin(half_theta) / sqrt(theta_sq);
  end if;
  q[1] := B;
  q[2] := A * v[1];
  q[3] := A * v[2];
  q[4] := A * v[3];
end exp_map;
