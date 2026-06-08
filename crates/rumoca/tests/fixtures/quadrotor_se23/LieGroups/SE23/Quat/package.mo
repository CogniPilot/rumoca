within LieGroups.SE23;
package Quat "SE_2(3) with quaternion rotation parameterization"
  annotation(Documentation(info="<html>
    <p>Group element: X = {px,py,pz, vx,vy,vz, qw,qx,qy,qz} (10 params).</p>
    <p>Lie algebra: xi = {vb_x,vb_y,vb_z, ab_x,ab_y,ab_z, omega_x,omega_y,omega_z} (9 params).</p>
    <p>Ordering: position, velocity, rotation (group); velocity, acceleration, rotation (algebra).</p>
  </html>"));
end Quat;
