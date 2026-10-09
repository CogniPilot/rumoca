// Generated from CogniPilot/modelica_models cb132c87a9e00289bbac11642110976248734878; edit the canonical packages there.
// Continuous error dynamics for the RGB-D / airframe IMU teaching filter.
// Error order: world dp, world dv, right-local dtheta, body dba, body dbg.
// R_true = R * Exp(dtheta); measurements are true body values + bias + noise.
// Noise order: acceleration white, gyro white, acceleration bias walk,
// gyro bias walk. These conventions differ from a right SE_2(3) filter.
// A host retains the filter state; all F/G mathematics below is Modelica.
model ES15Dynamics
  pure function Matrices
    input Real rotation[3,3] = identity(3);
    input Real force[3] = {0.0,0.0,9.81};
    input Real omega[3] = {0.0,0.0,0.0};
    output Real F[15,15];
    output Real G[15,12];
  protected
    Real forceSkew[3,3];
    Real omegaSkew[3,3];
  algorithm
    forceSkew := [0.0,-force[3],force[2];
                  force[3],0.0,-force[1];
                  -force[2],force[1],0.0];
    omegaSkew := [0.0,-omega[3],omega[2];
                  omega[3],0.0,-omega[1];
                  -omega[2],omega[1],0.0];
    F := fill(0.0,15,15);
    G := fill(0.0,15,12);
    F[1:3,4:6] := identity(3);
    // Keep the original three-term contraction order.
    for i in 1:3 loop
      for j in 1:3 loop
        F[i+3,j+6] := -(rotation[i,1]*forceSkew[1,j]
                         +rotation[i,2]*forceSkew[2,j]
                         +rotation[i,3]*forceSkew[3,j]);
      end for;
    end for;
    F[4:6,10:12] := -rotation;
    F[7:9,7:9] := -omegaSkew;
    F[7:9,13:15] := -identity(3);
    G[4:6,1:3] := -rotation;
    G[7:9,4:6] := -identity(3);
    G[10:12,7:9] := identity(3);
    G[13:15,10:12] := identity(3);
  end Matrices;

  input Real rotation[3,3] = identity(3);
  input Real force[3] = {0.0,0.0,9.81};
  input Real omega[3] = {0.0,0.0,0.0};
  output Real F[15,15];
  output Real G[15,12];
algorithm
  (F,G) := Matrices(rotation,force,omega);
end ES15Dynamics;
