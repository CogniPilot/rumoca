// Ground contact for MultiBody bodies, written as plain Modelica math.
//
// Each body carries a list of contact spheres fixed in its frame_a. Against the ground
// plane z = 0 every common shape reduces exactly to spheres: a sphere is itself, a capsule
// is the two spheres at the ends of its segment (the lowest point of a segment is always
// an endpoint), and a box is its eight corners with radius zero.
//
// One GroundContactBody per body sums the contact force and torque of all its spheres in a
// function and applies them through a single WorldForceAndTorque. The function first skips
// spheres that are clear of the ground (the broad phase), then applies a smooth
// Hunt-Crossley law with regularized Coulomb friction to the rest. Branches inside a
// function do not create events, and a skipped sphere would contribute zero force anyway,
// so the whole contact model stays one smooth ODE that can be differentiated end to end.

model GroundContactBody "Ground contact of one rigid body: all its spheres, one force and torque at frame_a"
  parameter Integer n "Number of contact spheres";
  parameter Real centre[n, 3] "Sphere centres in frame_a [m]";
  parameter Real radius[n] "Sphere radii [m]";
  parameter Real k = 1e5 "Contact stiffness per sphere [N/m]";
  parameter Real d = 0.5 "Hunt-Crossley damping [s/m]";
  parameter Real mu = 0.8 "Friction coefficient";
  parameter Real eps = 1e-4 "Penetration smoothing [m]";
  parameter Real v_eps = 0.01 "Friction regularization speed [m/s]";
  input Real v0[3] "Velocity of the frame_a origin, world frame [m/s]";
  MultiBodyLie.Interfaces.Frame_a frame_a;
  MultiBodyLie.Forces.WorldForceAndTorque wrench(resolveInFrame = MultiBodyLie.Types.ResolveInFrameB.world, animation = false);
  Real f[3] "Total contact force, world frame [N]";
  Real tau[3] "Total contact torque about the frame_a origin, world frame [N.m]";
  Real fz "Total normal force [N]";
equation
  connect(frame_a, wrench.frame_b);
  (f, tau) = groundWrench(frame_a.r_0, frame_a.R.T, frame_a.R.w, v0, centre, radius, k, d, mu, eps, v_eps);
  fz = f[3];
  wrench.force = f;
  wrench.torque = tau;
end GroundContactBody;

function groundWrench "Contact force and torque of a body's spheres against the plane z = 0"
  input Real r0[3] "frame_a origin, world [m]";
  input Real T[3, 3] "World-to-frame_a rotation";
  input Real w[3] "Angular velocity, resolved in frame_a [rad/s]";
  input Real v0[3] "frame_a origin velocity, world [m/s]";
  input Real centre[:, 3] "Sphere centres in frame_a [m]";
  input Real radius[size(centre, 1)] "Sphere radii [m]";
  input Real k;
  input Real d;
  input Real mu;
  input Real eps;
  input Real v_eps;
  output Real f[3] "Force, world [N]";
  output Real tau[3] "Torque about r0, world [N.m]";
protected
  Real arm[3] "Sphere centre relative to r0, world [m]";
  Real lowest[3] "Lowest point of the sphere relative to r0, world [m]";
  Real v[3] "Velocity of the lowest point, world [m/s] (includes spin, so rolling has no slip)";
  Real fn;
  Real vt;
  Real fi[3];
algorithm
  f := zeros(3);
  tau := zeros(3);
  for i in 1:size(centre, 1) loop
    arm := transpose(T) * centre[i, :];
    // Broad phase: a sphere more than 50 smoothing lengths above the ground carries no force.
    if r0[3] + arm[3] - radius[i] < 50 * eps then
      lowest := arm - {0, 0, radius[i]};
      v := v0 + cross(transpose(T) * w, lowest);
      fn := k * contactSoftplus(-(r0[3] + lowest[3]), eps) * contactSoftplus(1 - d * v[3], 0.05);
      vt := sqrt(v[1] ^ 2 + v[2] ^ 2 + v_eps ^ 2);
      fi := {-mu * fn * v[1] / vt, -mu * fn * v[2] / vt, fn};
      f := f + fi;
      tau := tau + cross(lowest, fi);
    end if;
  end for;
end groundWrench;

function contactSoftplus "Smooth max(x, 0) with transition width s; overflow-safe"
  input Real x;
  input Real s;
  output Real y;
algorithm
  if x > 0 then
    y := x + s * log(1 + exp(-x / s));
  else
    y := s * log(1 + exp(x / s));
  end if;
end contactSoftplus;
