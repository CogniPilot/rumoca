// Generated from CogniPilot/modelica_models cb132c87a9e00289bbac11642110976248734878; edit the canonical packages there.
// One held-IMU substep for the existing ES15 filter convention.
// Rotation maps body FLU into the gravity-aligned map frame. Biases stay body-local.
// The caller subdivides a frame to h <= 20 ms and |omega|*h <= 0.1 rad.
// Pure Modelica mathematics; this component is not yet a production estimator.
model ES15NominalPrediction
  pure function Predict
    input Real rotation[3,3] = identity(3);
    input Real position[3] = {0.0,0.0,0.0};
    input Real velocity[3] = {0.0,0.0,0.0};
    input Real accel[3] = {0.0,0.0,9.81};
    input Real gyro[3] = {0.0,0.0,0.0};
    input Real accel_bias[3] = {0.0,0.0,0.0};
    input Real gyro_bias[3] = {0.0,0.0,0.0};
    input Real gravity[3] = {0.0,0.0,-9.81};
    input Real h = 1.0/90.0;
    output Real force[3]; output Real omega[3];
    output Real middle_rotation[3,3]; output Real next_rotation[3,3];
    output Real next_position[3]; output Real next_velocity[3];
    output Real valid;
  protected
    Real angle; Real a; Real b; Real middle_a; Real middle_b;
    Real W[3,3]; Real W2[3,3];
    Real increment[3,3]; Real middle_increment[3,3]; Real proposed_rotation[3,3];
    Real acceleration[3];
  algorithm
    force := accel-accel_bias;
    omega := gyro-gyro_bias;
    angle := sqrt(omega[1]^2+omega[2]^2+omega[3]^2)*h;
    valid := if noEvent(h > 0.0 and h <= 0.02 and angle <= 0.1) then 1.0 else 0.0;
    // Protected denominators also keep unselected small-angle branches finite.
    a := if noEvent(angle < 1e-7) then 1.0 else sin(angle)/max(angle,1e-7);
    b := if noEvent(angle < 1e-7) then 0.5 else (1.0-cos(angle))/max(angle^2,1e-14);
    middle_a := if noEvent(angle*0.5 < 1e-7) then 1.0 else sin(angle*0.5)/max(angle*0.5,1e-7);
    middle_b := if noEvent(angle*0.5 < 1e-7) then 0.5 else (1.0-cos(angle*0.5))/max(angle^2*0.25,1e-14);
    W := [0.0,-omega[3]*h,omega[2]*h;
         omega[3]*h,0.0,-omega[1]*h;
         -omega[2]*h,omega[1]*h,0.0];
    W2 := W*W;
    increment := identity(3)+a*W+b*W2;
    middle_increment := identity(3)+middle_a*0.5*W+middle_b*0.25*W2;
    middle_rotation := rotation*middle_increment;
    proposed_rotation := rotation*increment;
    acceleration := middle_rotation*force+gravity;
    for i in 1:3 loop
      next_position[i] := if noEvent(valid > 0.5) then position[i]+velocity[i]*h+0.5*acceleration[i]*h^2 else position[i];
      next_velocity[i] := if noEvent(valid > 0.5) then velocity[i]+acceleration[i]*h else velocity[i];
      for j in 1:3 loop
        next_rotation[i,j] := if noEvent(valid > 0.5) then proposed_rotation[i,j] else rotation[i,j];
      end for;
    end for;
  end Predict;

  input Real rotation[3,3] = identity(3);
  input Real position[3] = {0.0,0.0,0.0};
  input Real velocity[3] = {0.0,0.0,0.0};
  input Real accel[3] = {0.0,0.0,9.81};
  input Real gyro[3] = {0.0,0.0,0.0};
  input Real accel_bias[3] = {0.0,0.0,0.0};
  input Real gyro_bias[3] = {0.0,0.0,0.0};
  input Real gravity[3] = {0.0,0.0,-9.81};
  input Real h = 1.0/90.0;
  output Real force[3]; output Real omega[3];
  output Real middle_rotation[3,3]; output Real next_rotation[3,3];
  output Real next_position[3]; output Real next_velocity[3];
  output Real valid;
algorithm
  (force,omega,middle_rotation,next_rotation,next_position,next_velocity,valid) :=
    Predict(rotation,position,velocity,accel,gyro,accel_bias,gyro_bias,gravity,h);
end ES15NominalPrediction;
