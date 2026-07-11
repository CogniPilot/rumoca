// Reusable-booster first-stage terminal landing demonstration.
//
// World frame: x downrange, y crossrange, z up.
// Body frame: z along the booster axis toward the interstage.

// This is an educational rigid-body/control model, not an operational launch-vehicle simulation.

function quinticFlatnessReference
  input Real timing[2] "{trajectory time, duration} [s]";
  input Real initial_state[9] "{position, velocity, acceleration}";
  input Real terminal_state[9] "{position, velocity, acceleration}";
  output Real trajectory[9] "{position, velocity, acceleration}";

protected
  Real tau;
  Real coefficients[6, 3];
  Real position_delta[3];
  Real velocity_delta[3];
  Real acceleration_delta[3];

algorithm
  tau := min(max(timing[1], 0.0), timing[2]);
  for axis in 1:3 loop
    position_delta[axis] := terminal_state[axis] - initial_state[axis]
      - initial_state[axis + 3] * timing[2]
      - 0.5 * initial_state[axis + 6] * timing[2]^2;
    velocity_delta[axis] := terminal_state[axis + 3]
      - initial_state[axis + 3] - initial_state[axis + 6] * timing[2];
    acceleration_delta[axis] := terminal_state[axis + 6] - initial_state[axis + 6];
    coefficients[1, axis] := initial_state[axis];
    coefficients[2, axis] := initial_state[axis + 3];
    coefficients[3, axis] := 0.5 * initial_state[axis + 6];
    coefficients[4, axis] := 10.0 * position_delta[axis] / timing[2]^3
      - 4.0 * velocity_delta[axis] / timing[2]^2
      + 0.5 * acceleration_delta[axis] / timing[2];
    coefficients[5, axis] := -15.0 * position_delta[axis] / timing[2]^4
      + 7.0 * velocity_delta[axis] / timing[2]^3
      - acceleration_delta[axis] / timing[2]^2;
    coefficients[6, axis] := 6.0 * position_delta[axis] / timing[2]^5
      - 3.0 * velocity_delta[axis] / timing[2]^4
      + 0.5 * acceleration_delta[axis] / timing[2]^3;
    trajectory[axis] := coefficients[1, axis]
      + coefficients[2, axis] * tau
      + coefficients[3, axis] * tau^2
      + coefficients[4, axis] * tau^3
      + coefficients[5, axis] * tau^4
      + coefficients[6, axis] * tau^5;
    trajectory[axis + 3] := coefficients[2, axis]
      + 2.0 * coefficients[3, axis] * tau
      + 3.0 * coefficients[4, axis] * tau^2
      + 4.0 * coefficients[5, axis] * tau^3
      + 5.0 * coefficients[6, axis] * tau^4;
    trajectory[axis + 6] := 2.0 * coefficients[3, axis]
      + 6.0 * coefficients[4, axis] * tau
      + 12.0 * coefficients[5, axis] * tau^2
      + 20.0 * coefficients[6, axis] * tau^3;
  end for;
end quinticFlatnessReference;

function quinticPlanFeasible
  input Real initial_state[9] "{position, velocity, acceleration}";
  input Real terminal_state[9] "{position, velocity, acceleration}";
  input Real feasibility_parameters[6]
    "{duration, mass, minimum thrust, maximum thrust, CG height, propellant}";
  output Real feasible "Reference feasibility indicator [0..1]";
protected
  constant Real gravity = 9.80665;
  constant Real specific_impulse = 282.0;
  constant Real tilt_limit = 20.0 * 3.141592653589793 / 180.0;
  constant Integer intervals = 20;
  Real trajectory[9];
  Real coefficients[6, 3];
  Real position_delta[3];
  Real velocity_delta[3];
  Real acceleration_delta[3];
  Real tau;
  Real force[3];
  Real thrust;
  Real tilt;
  Real propellant_required;
algorithm
  feasible := 1.0;
  propellant_required := 0.0;
  for axis in 1:3 loop
    position_delta[axis] := terminal_state[axis] - initial_state[axis]
      - initial_state[axis + 3] * feasibility_parameters[1]
      - 0.5 * initial_state[axis + 6] * feasibility_parameters[1]^2;
    velocity_delta[axis] := terminal_state[axis + 3]
      - initial_state[axis + 3]
      - initial_state[axis + 6] * feasibility_parameters[1];
    acceleration_delta[axis] := terminal_state[axis + 6] - initial_state[axis + 6];
    coefficients[1, axis] := initial_state[axis];
    coefficients[2, axis] := initial_state[axis + 3];
    coefficients[3, axis] := 0.5 * initial_state[axis + 6];
    coefficients[4, axis] := 10.0 * position_delta[axis] / feasibility_parameters[1]^3
      - 4.0 * velocity_delta[axis] / feasibility_parameters[1]^2
      + 0.5 * acceleration_delta[axis] / feasibility_parameters[1];
    coefficients[5, axis] := -15.0 * position_delta[axis] / feasibility_parameters[1]^4
      + 7.0 * velocity_delta[axis] / feasibility_parameters[1]^3
      - acceleration_delta[axis] / feasibility_parameters[1]^2;
    coefficients[6, axis] := 6.0 * position_delta[axis] / feasibility_parameters[1]^5
      - 3.0 * velocity_delta[axis] / feasibility_parameters[1]^4
      + 0.5 * acceleration_delta[axis] / feasibility_parameters[1]^3;
  end for;
  for sample_index in 0:intervals loop
    tau := feasibility_parameters[1] * sample_index / intervals;
    for axis in 1:3 loop
      trajectory[axis] := coefficients[1, axis]
        + coefficients[2, axis] * tau
        + coefficients[3, axis] * tau^2
        + coefficients[4, axis] * tau^3
        + coefficients[5, axis] * tau^4
        + coefficients[6, axis] * tau^5;
      trajectory[axis + 6] := 2.0 * coefficients[3, axis]
        + 6.0 * coefficients[4, axis] * tau
        + 12.0 * coefficients[5, axis] * tau^2
        + 20.0 * coefficients[6, axis] * tau^3;
    end for;
    force[1] := feasibility_parameters[2] * trajectory[7];
    force[2] := feasibility_parameters[2] * trajectory[8];
    force[3] := feasibility_parameters[2] * (trajectory[9] + gravity);
    thrust := sqrt(force * force);
    tilt := atan2(sqrt(force[1]^2 + force[2]^2), force[3]);
    if thrust < feasibility_parameters[3] or thrust > feasibility_parameters[4]
        or force[3] <= 0.0 or tilt > tilt_limit
        or trajectory[3] < 0.5 * feasibility_parameters[5] then
      feasible := 0.0;
    end if;
    propellant_required := propellant_required
      + thrust * feasibility_parameters[1] / intervals / (specific_impulse * gravity)
        * (if sample_index == 0 or sample_index == intervals then 0.5 else 1.0);
  end for;
  if propellant_required > feasibility_parameters[6] then
    feasible := 0.0;
  end if;
end quinticPlanFeasible;

function movingDeckCgTarget
  input Real target_time "Terminal mission time [s]";
  input Real heave_amplitude "Deck heave amplitude [m]";
  input Real roll_amplitude "Deck roll amplitude [rad]";
  input Real pitch_amplitude "Deck pitch amplitude [rad]";
  input Real wave_period "Deck wave period [s]";
  input Real cg_height "Target CG height along the deck normal [m]";
  output Real target[12] "{position, velocity, acceleration, deck normal}";

protected
  constant Real pi = 3.141592653589793;
  Real heave_rate;
  Real roll_rate;
  Real pitch_rate;
  Real roll_acceleration;
  Real pitch_acceleration;
  Real roll;
  Real pitch;
  Real normal[3];
  Real offset[3];
  Real omega[3];
  Real alpha[3];
  Real deck_position[3];
  Real deck_velocity[3];
  Real deck_acceleration[3];

algorithm
  heave_rate := 2.0 * pi / wave_period;
  roll_rate := heave_rate;
  pitch_rate := 1.66 * pi / wave_period;
  roll := roll_amplitude * sin(roll_rate * target_time);
  pitch := pitch_amplitude * sin(pitch_rate * target_time);
  roll_acceleration := -roll_amplitude * roll_rate^2
    * sin(roll_rate * target_time);
  pitch_acceleration := -pitch_amplitude * pitch_rate^2
    * sin(pitch_rate * target_time);
  omega := {
    roll_amplitude * roll_rate * cos(roll_rate * target_time) * cos(pitch),
    pitch_amplitude * pitch_rate * cos(pitch_rate * target_time),
    -roll_amplitude * roll_rate * cos(roll_rate * target_time) * sin(pitch)};
  alpha := {
    roll_acceleration * cos(pitch) - omega[1] * omega[2] * tan(pitch),
    pitch_acceleration,
    -roll_acceleration * sin(pitch)
      - roll_amplitude * roll_rate * cos(roll_rate * target_time)
        * cos(pitch) * omega[2]};
  normal := {sin(pitch) * cos(roll), -sin(roll), cos(pitch) * cos(roll)};
  offset := cg_height * normal;
  deck_position := {0, 0, heave_amplitude * sin(heave_rate * target_time)};
  deck_velocity := {0, 0,
    heave_amplitude * heave_rate * cos(heave_rate * target_time)};
  deck_acceleration := {0, 0,
    -heave_amplitude * heave_rate^2 * sin(heave_rate * target_time)};
  target[1] := deck_position[1] + offset[1];
  target[2] := deck_position[2] + offset[2];
  target[3] := deck_position[3] + offset[3];
  target[4] := deck_velocity[1] + cross(omega, offset)[1];
  target[5] := deck_velocity[2] + cross(omega, offset)[2];
  target[6] := deck_velocity[3] + cross(omega, offset)[3];
  target[7] := deck_acceleration[1] + cross(alpha, offset)[1]
    + cross(omega, cross(omega, offset))[1];
  target[8] := deck_acceleration[2] + cross(alpha, offset)[2]
    + cross(omega, cross(omega, offset))[2];
  target[9] := deck_acceleration[3] + cross(alpha, offset)[3]
    + cross(omega, cross(omega, offset))[3];
  target[10] := normal[1];
  target[11] := normal[2];
  target[12] := normal[3];
end movingDeckCgTarget;

function triangleSupportMargin
  input Real vertices[2, 3] "Counterclockwise support-triangle vertices [m]";
  input Real point[2] "CG projection in deck coordinates [m]";
  output Real margin "Minimum signed edge distance [m]";
protected
  constant Integer next_vertex[3] = {2, 3, 1};
  Real edge[2];
  Real relative_point[2];
algorithm
  margin := 1.0e9;
  for vertex in 1:3 loop
    edge := vertices[:, next_vertex[vertex]] - vertices[:, vertex];
    relative_point := point - vertices[:, vertex];
    margin := min(
      margin,
      (edge[1] * relative_point[2] - edge[2] * relative_point[1])
        / sqrt(edge * edge + 1.0e-12));
  end for;
end triangleSupportMargin;

function loadedFootSupportMargin
  input Real foot_position_deck[3, 4] "Foot positions in deck coordinates [m]";
  input Real point_deck[2] "CG projection in deck coordinates [m]";
  input Real loaded[4] "Meaningfully loaded-foot indicators";
  output Real margin "Minimum signed support-polygon margin [m]";

protected
  constant Integer hull_order[5] = {1, 3, 2, 4, 1};
  Integer first;
  Integer second;
  Real edge_vector[2];
  Real relative_point[2];

algorithm
  if sum(loaded) < 2.5 then
    margin := -1.0;
  elseif loaded[1] < 0.5 then
    margin := triangleSupportMargin([
      foot_position_deck[1, 3], foot_position_deck[1, 2], foot_position_deck[1, 4];
      foot_position_deck[2, 3], foot_position_deck[2, 2], foot_position_deck[2, 4]],
      point_deck);
  elseif loaded[2] < 0.5 then
    margin := triangleSupportMargin([
      foot_position_deck[1, 1], foot_position_deck[1, 3], foot_position_deck[1, 4];
      foot_position_deck[2, 1], foot_position_deck[2, 3], foot_position_deck[2, 4]],
      point_deck);
  elseif loaded[3] < 0.5 then
    margin := triangleSupportMargin([
      foot_position_deck[1, 1], foot_position_deck[1, 2], foot_position_deck[1, 4];
      foot_position_deck[2, 1], foot_position_deck[2, 2], foot_position_deck[2, 4]],
      point_deck);
  elseif loaded[4] < 0.5 then
    margin := triangleSupportMargin([
      foot_position_deck[1, 1], foot_position_deck[1, 3], foot_position_deck[1, 2];
      foot_position_deck[2, 1], foot_position_deck[2, 3], foot_position_deck[2, 2]],
      point_deck);
  else
    margin := 1.0e9;
    for edge in 1:4 loop
      first := hull_order[edge];
      second := hull_order[edge + 1];
      edge_vector := {
        foot_position_deck[1, second], foot_position_deck[2, second]}
        - {foot_position_deck[1, first], foot_position_deck[2, first]};
      relative_point := point_deck
        - {foot_position_deck[1, first], foot_position_deck[2, first]};
      margin := min(
        margin,
        (edge_vector[1] * relative_point[2]
          - edge_vector[2] * relative_point[1])
          / sqrt(edge_vector * edge_vector + 1.0e-12));
    end for;
  end if;
end loadedFootSupportMargin;

function loadedFootMaximum
  input Real values[4] "Per-foot nonnegative quantity";
  input Real loaded[4] "Meaningfully loaded-foot indicators";
  input Real contacting[4] "Physical foot-contact indicators";
  input Real fallback "Value used when contact is not through a landing foot";
  output Real maximum "Maximum over loaded or physically contacting feet";
protected
  Real selected[4];
algorithm
  selected := {
    if loaded[foot] > 0.0 or contacting[foot] > 0.0 then 1.0 else 0.0
    for foot in 1:4};
  maximum := if sum(selected) > 0.5 then max(values .* selected) else fallback;
end loadedFootMaximum;

function touchdownKinematicsSafe
  input Real normal_speed "Maximum active foot-contact normal speed [m/s]";
  input Real tangential_speed "Maximum active foot-contact tangential speed [m/s]";
  input Real tilt_deg "Body tilt from deck normal [deg]";
  output Real safe "Safe touchdown-kinematics indicator [0..1]";

algorithm
  safe := if normal_speed <= 2.0
    and tangential_speed <= 1.5
    and tilt_deg <= 10.0 then 1.0 else 0.0;
end touchdownKinematicsSafe;

function touchdownAcceptance
  input Real supporting_legs "Number of meaningfully loaded feet";
  input Real support_margin "CG support-polygon margin [m]";
  input Real normal_speed "Maximum active foot-contact normal speed [m/s]";
  input Real tangential_speed "Maximum active foot-contact tangential speed [m/s]";
  input Real tilt_deg "Body tilt from deck normal [deg]";
  output Real accepted "Safe-touchdown indicator [0..1]";

algorithm
  accepted := if supporting_legs > 2.5
    and support_margin >= 0.25
    and touchdownKinematicsSafe(
      normal_speed, tangential_speed, tilt_deg) > 0.5 then 1.0 else 0.0;
end touchdownAcceptance;

function tangentialContactForce
  input Real normal_force "Compressive contact force [N]";
  input Real contact_gate "Regularized contact activation [0..1]";
  input Real tangent_velocity[3] "Foot velocity tangent to the deck [m/s]";
  input Real viscous_damping "Trial viscous damping [N*s/m]";
  input Real friction_coefficient "Coulomb friction coefficient";
  output Real force[3] "Deck-tangent friction force [N]";
protected
  Real trial_force[3];
  Real trial_magnitude;
  Real friction_scale;
algorithm
  trial_force := (-contact_gate * viscous_damping) * tangent_velocity;
  trial_magnitude := sqrt(trial_force * trial_force);
  friction_scale := min(
    1.0,
    friction_coefficient * max(normal_force, 0.0)
      / max(trial_magnitude, 1.0e-12));
  force := friction_scale * trial_force;
end tangentialContactForce;

function scheduledRcsAvailability
  input Real altitude "Engine-plane altitude above the deck [m]";
  input Real mission_gate "Mission actuator gate [0..1]";
  output Real availability "Altitude-scheduled RCS availability [0..1]";
algorithm
  availability := min(max(mission_gate, 0.0), 1.0)
    * min(max((altitude - 80.0) / 40.0, 0.0), 1.0);
end scheduledRcsAvailability;

function rcsPropellantMassFlow
  input Real force_b[3] "Actual aggregate body force [N]";
  input Real roll_moment "Actual roll-couple moment [N*m]";
  output Real mass_flow "RCS propellant mass flow [kg/s]";
protected
  constant Real gravity = 9.80665;
  constant Real specific_impulse = 60.0;
  constant Real roll_radius = 1.35;
algorithm
  mass_flow := (
    abs(force_b[1]) + abs(force_b[2])
      + abs(roll_moment) / roll_radius) / (specific_impulse * gravity);
end rcsPropellantMassFlow;

function thrustDirectionQuaternion
  input Real force_w[3] "Desired force in world coordinates [N]";
  output Real q[4] "Body-to-world quaternion aligning body +z with force";

protected
  Real magnitude;
  Real b3[3];
  Real denominator;

algorithm
  magnitude := sqrt(force_w * force_w + 1.0e-12);
  b3 := force_w / magnitude;
  denominator := 1.0;
  if b3[3] > -0.999999 then
    denominator := sqrt(2.0 * (1.0 + b3[3]));
    q := {0.5 * denominator, -b3[2] / denominator, b3[1] / denominator, 0.0};
  else
    q := {0.0, 1.0, 0.0, 0.0};
  end if;
end thrustDirectionQuaternion;

function se23TranslationalCorrection
  input Real error[9] "SE_2(3) logarithmic tracking error";
  input Real position_gain_scale[3] "Per-axis position-feedback gain multipliers";
  input Real velocity_gain_scale[3] "Per-axis velocity-feedback gain multipliers";
  output Real correction[3] "Body-frame position/velocity correction";

protected
  constant Real position_gain[3] = {0.04, 0.04, 0.08};
  constant Real velocity_gain[3] = {0.35, 0.35, 1.40};
  Real jacobian[9, 9];
  Real weighted_error[9];
  Real feedback[9];

algorithm
  weighted_error := cat(
    1,
    position_gain .* position_gain_scale .* error[1:3],
    velocity_gain .* velocity_gain_scale .* error[4:6],
    zeros(3));
  jacobian := LieGroups.SE23.Quat.left_jacobian(error);
  feedback := jacobian * weighted_error;
  correction := feedback[1:3] + feedback[4:6];
end se23TranslationalCorrection;

function quaternionReferenceRate
  input Real previous_quat[4] "Previous body-to-world reference quaternion";
  input Real current_quat[4] "Current body-to-world reference quaternion";
  input Real sample_period "Reference sample period [s]";
  output Real body_rate[3] "Finite-difference reference body rate [rad/s]";
protected
  Real previous_norm;
  Real relative_quat[4];
algorithm
  previous_norm := sqrt(previous_quat * previous_quat);
  if previous_norm < 0.5 or sample_period <= 1.0e-9 then
    body_rate := {0, 0, 0};
  else
    relative_quat := LieGroups.SO3.Quat.product(
      LieGroups.SO3.Quat.inverse(previous_quat), current_quat);
    body_rate := LieGroups.SO3.Quat.log_map(relative_quat) / sample_period;
  end if;
end quaternionReferenceRate;

function slewLimitedQuaternionReference
  input Real previous_quat[4] "Previous body-to-world reference quaternion";
  input Real desired_quat[4] "Unconstrained body-to-world reference quaternion";
  input Real sample_period "Reference sample period [s]";
  input Real max_rate "Maximum reference rotation rate [rad/s]";
  output Real limited_quat[4] "Rate-limited body-to-world reference quaternion";
protected
  Real previous_norm;
  Real relative_quat[4];
  Real rotation_step[3];
  Real rotation_norm;
  Real step_scale;
algorithm
  previous_norm := sqrt(previous_quat * previous_quat);
  if previous_norm < 0.5 then
    limited_quat := LieGroups.SO3.Quat.normalize(desired_quat);
  elseif sample_period <= 1.0e-9 or max_rate <= 0.0 then
    limited_quat := LieGroups.SO3.Quat.normalize(previous_quat);
  else
    relative_quat := LieGroups.SO3.Quat.product(
      LieGroups.SO3.Quat.inverse(previous_quat), desired_quat);
    rotation_step := LieGroups.SO3.Quat.log_map(relative_quat);
    rotation_norm := sqrt(rotation_step * rotation_step);
    step_scale := min(
      1.0, max_rate * sample_period / max(rotation_norm, 1.0e-12));
    limited_quat := LieGroups.SO3.Quat.normalize(
      LieGroups.SO3.Quat.product(
        previous_quat,
        LieGroups.SO3.Quat.exp_map(step_scale * rotation_step)));
  end if;
end slewLimitedQuaternionReference;

function expressReferenceRateInBody
  input Real actual_quat[4] "Actual body-to-world quaternion";
  input Real reference_quat[4] "Reference body-to-world quaternion";
  input Real reference_rate[3] "Angular rate expressed in the reference body";
  output Real actual_body_rate[3] "Reference angular rate expressed in the actual body";
protected
  Real attitude_error_quat[4];
algorithm
  attitude_error_quat := LieGroups.SO3.Quat.product(
    LieGroups.SO3.Quat.inverse(actual_quat), reference_quat);
  actual_body_rate := LieGroups.SO3.Quat.rotate(
    attitude_error_quat, reference_rate);
end expressReferenceRateInBody;

function packSe23ControllerCommand
  input Real reference_quat[4];
  input Real error[9];
  input Real acceleration[3];
  input Real force[3];
  input Real moment[3];
  input Real force_quat[4];
  output Real command[27]
    "{reference quaternion, group error, acceleration, force, moment, thrust, force quaternion}";
algorithm
  for index in 1:4 loop
    command[index] := reference_quat[index];
  end for;
  for index in 1:9 loop
    command[index + 4] := error[index];
  end for;
  for index in 1:3 loop
    command[index + 13] := acceleration[index];
    command[index + 16] := force[index];
    command[index + 19] := moment[index];
  end for;
  command[23] := sqrt(force * force);
  for index in 1:4 loop
    command[index + 23] := force_quat[index];
  end for;
end packSe23ControllerCommand;

function reusableBoosterTranslationalControlLaw
  input Real reference_acceleration[3] "World-frame reference acceleration [m/s^2]";
  input Real world_correction[3] "World-frame geometric feedback correction [m/s^2]";
  input Real drag_w[3] "World-frame aerodynamic force [N]";
  input Real vehicle_mass "Vehicle mass [kg]";
  output Real control[6] "{acceleration[3], force[3]}";
protected
  Real acceleration[3];
  Real force_unconstrained[3];
  Real force_direction_scale;
  Real force[3];
algorithm
  acceleration := reference_acceleration + world_correction;
  force_unconstrained := vehicle_mass * {
    acceleration[1],
    acceleration[2],
    acceleration[3] + 9.80665} - drag_w;
  force_direction_scale := min(
    1.0,
    tan(0.3490658503988659)
      * max(force_unconstrained[3], 0.35 * vehicle_mass * 9.80665)
      / sqrt(force_unconstrained[1]^2 + force_unconstrained[2]^2 + 1.0));
  force := {
    force_direction_scale * force_unconstrained[1],
    force_direction_scale * force_unconstrained[2],
    max(force_unconstrained[3], 0.35 * vehicle_mass * 9.80665)};
  control := {
    acceleration[1], acceleration[2], acceleration[3],
    force[1], force[2], force[3]};
end reusableBoosterTranslationalControlLaw;

function reusableBoosterRotationalControlLaw
  input Real attitude_error[3] "Body-frame logarithmic attitude error";
  input Real omega[3] "Actual body rate [rad/s]";
  input Real reference_omega[3] "Reference rate expressed in the actual body [rad/s]";
  input Real gain_scale[2] "{attitude gain, rate gain}";
  input Real inertia[3] "Principal moments of inertia [kg*m^2]";
  output Real moment[3] "Commanded body moment [N*m]";
protected
  Real angular_momentum[3];
  Real gyroscopic_moment[3];
  Real reference_transport_moment[3];
algorithm
  angular_momentum := inertia .* omega;
  gyroscopic_moment := {
    omega[2] * angular_momentum[3] - omega[3] * angular_momentum[2],
    omega[3] * angular_momentum[1] - omega[1] * angular_momentum[3],
    omega[1] * angular_momentum[2] - omega[2] * angular_momentum[1]};
  reference_transport_moment := inertia .* {
    omega[2] * reference_omega[3] - omega[3] * reference_omega[2],
    omega[3] * reference_omega[1] - omega[1] * reference_omega[3],
    omega[1] * reference_omega[2] - omega[2] * reference_omega[1]};
  moment := gain_scale[1] * {3200000.0, 3200000.0, 90000.0} .* attitude_error
    - gain_scale[2] * {7500000.0, 7500000.0, 65000.0} .* (omega - reference_omega)
    + gyroscopic_moment - reference_transport_moment;
end reusableBoosterRotationalControlLaw;

function reusableBoosterEmbeddedControlLaw
  input Real translation[10]
    "{reference acceleration[3], world correction[3], drag[3], mass}";
  input Real rotation[9] "{attitude error[3], omega[3], reference omega[3]}";
  input Real gains[2] "{attitude gain, rate gain}";
  input Real inertia[3] "Principal moments of inertia [kg*m^2]";
  output Real control[9] "{acceleration[3], force[3], moment[3]}";
protected
  Real translation_control[6];
  Real moment[3];
algorithm
  translation_control := reusableBoosterTranslationalControlLaw(
    translation[1:3], translation[4:6], translation[7:9], translation[10]);
  moment := reusableBoosterRotationalControlLaw(
    rotation[1:3], rotation[4:6], rotation[7:9], gains, inertia);
  control := {
    translation_control[1], translation_control[2], translation_control[3],
    translation_control[4], translation_control[5], translation_control[6],
    moment[1], moment[2], moment[3]};
end reusableBoosterEmbeddedControlLaw;

function sampledSe23Controller
  input Real state[17] "{position, velocity, quaternion, omega, previous force quaternion}";
  input Real reference[9] "{position, velocity, acceleration}";
  input Real gains[8] "{position gain[3], velocity gain[3], attitude gain, rate gain}";
  input Real vehicle[8] "{sample period, mass, inertia[3], world drag[3]}";
  output Real command[27]
    "{reference quaternion, group error, acceleration, force, moment, thrust, force quaternion}";

protected
  constant Real gravity = 9.80665;
  constant Real attitude_rate_limit = 0.05;
  Real reference_force_direction[3];
  Real reference_quat[4];
  Real actual_group[10];
  Real reference_group[10];
  Real error[9];
  Real correction[3];
  Real rotation[3, 3];
  Real translation_control[6];
  Real acceleration[3];
  Real force[3];
  Real force_quat_desired[4];
  Real force_quat[4];
  Real attitude_error[3];
  Real reference_omega[3];
  Real reference_omega_actual_body[3];
  Real moment[3];

algorithm
  reference_force_direction := reference[7:9] + {0, 0, gravity};
  reference_quat := thrustDirectionQuaternion(reference_force_direction);
  actual_group := cat(1, state[1:3], state[4:6], state[7:10]);
  reference_group := cat(1, reference[1:3], reference[4:6], reference_quat);
  error := LieGroups.SE23.Quat.log_map(
    LieGroups.SE23.Quat.product(
      LieGroups.SE23.Quat.inverse(actual_group),
      reference_group));
  correction := se23TranslationalCorrection(
    error,
    gains[1:3],
    gains[4:6]);
  rotation := LieGroups.SO3.Quat.to_DCM(state[7:10]);
  translation_control := reusableBoosterTranslationalControlLaw(
    reference[7:9], rotation * correction, vehicle[6:8], vehicle[2]);
  acceleration := translation_control[1:3];
  force := translation_control[4:6];
  force_quat_desired := thrustDirectionQuaternion(force);
  force_quat := slewLimitedQuaternionReference(
    state[14:17],
    force_quat_desired,
    vehicle[1],
    attitude_rate_limit);
  attitude_error := LieGroups.SO3.Quat.log_map(
    LieGroups.SO3.Quat.product(
      LieGroups.SO3.Quat.inverse(state[7:10]),
      force_quat));
  reference_omega := quaternionReferenceRate(
    state[14:17], force_quat, vehicle[1]);
  reference_omega_actual_body := expressReferenceRateInBody(
    state[7:10], force_quat, reference_omega);
  moment := reusableBoosterRotationalControlLaw(
    attitude_error,
    state[11:13],
    reference_omega_actual_body,
    gains[7:8],
    vehicle[3:5]);
  command := packSe23ControllerCommand(
    reference_quat, error, acceleration, force, moment, force_quat);
end sampledSe23Controller;

function initialSe23EstimatorPacket
  input Real position[3];
  input Real velocity[3];
  output Real packet[91]
    "SE_2(3) state followed by column-major 9x9 tangent covariance";
algorithm
  for index in 1:91 loop
    packet[index] := 0.0;
  end for;
  packet[1] := position[1];
  packet[2] := position[2];
  packet[3] := position[3];
  packet[4] := velocity[1];
  packet[5] := velocity[2];
  packet[6] := velocity[3];
  packet[7] := 1.0;
  packet[8] := 0.0;
  packet[9] := 0.0;
  packet[10] := 0.0;
  for axis in 1:3 loop
    packet[10 + (axis - 1) * 9 + axis] := 4.0;
    packet[10 + (axis + 2) * 9 + axis + 3] := 1.0;
    packet[10 + (axis + 5) * 9 + axis + 6] := 0.01;
  end for;
end initialSe23EstimatorPacket;

function symmetricPositiveDefiniteInverse3
  input Real matrix[3, 3] "Nominal symmetric positive-definite matrix";
  output Real inverse[3, 3] "Jitter-regularized inverse";
protected
  Real regularized[3, 3];
  Real lower[3, 3];
  Real forward[3];
  Real solution[3];
  Real jitter;
algorithm
  jitter := max(
    1.0e-12,
    1.0e-9 * (abs(matrix[1, 1]) + abs(matrix[2, 2])
      + abs(matrix[3, 3]) + 1.0));
  regularized := 0.5 * (matrix + transpose(matrix));
  for axis in 1:3 loop
    regularized[axis, axis] := regularized[axis, axis] + jitter;
  end for;
  lower := zeros(3, 3);
  lower[1, 1] := sqrt(max(regularized[1, 1], jitter));
  lower[2, 1] := regularized[2, 1] / lower[1, 1];
  lower[3, 1] := regularized[3, 1] / lower[1, 1];
  lower[2, 2] := sqrt(max(
    regularized[2, 2] - lower[2, 1]^2,
    jitter));
  lower[3, 2] := (regularized[3, 2]
    - lower[3, 1] * lower[2, 1]) / lower[2, 2];
  lower[3, 3] := sqrt(max(
    regularized[3, 3] - lower[3, 1]^2 - lower[3, 2]^2,
    jitter));

  for col in 1:3 loop
    forward[1] := (if col == 1 then 1.0 else 0.0) / lower[1, 1];
    forward[2] := ((if col == 2 then 1.0 else 0.0)
      - lower[2, 1] * forward[1]) / lower[2, 2];
    forward[3] := ((if col == 3 then 1.0 else 0.0)
      - lower[3, 1] * forward[1]
      - lower[3, 2] * forward[2]) / lower[3, 3];
    solution[3] := forward[3] / lower[3, 3];
    solution[2] := (forward[2] - lower[3, 2] * solution[3])
      / lower[2, 2];
    solution[1] := (forward[1] - lower[2, 1] * solution[2]
      - lower[3, 1] * solution[3]) / lower[1, 1];
    for row in 1:3 loop
      inverse[row, col] := solution[row];
    end for;
  end for;
end symmetricPositiveDefiniteInverse3;

function unpackSe23EstimatorCovariance
  input Real packet[91];
  output Real covariance[9, 9];
algorithm
  for row in 1:9 loop
    for col in 1:9 loop
      covariance[row, col] := packet[10 + (col - 1) * 9 + row];
    end for;
  end for;
end unpackSe23EstimatorCovariance;

function predictSe23EstimatorState
  input Real previous[10];
  input Real dt;
  input Real gyro[3];
  input Real specific_force[3];
  output Real predicted[10];
protected
  constant Real gravity = 9.80665;
  Real left_increment[9];
  Real right_increment[9];
  Real coupling[2, 2];
algorithm
  left_increment := cat(
    1, zeros(3), specific_force * dt, gyro * dt);
  right_increment := {0, 0, 0, 0, 0, -gravity * dt, 0, 0, 0};
  coupling := [0, dt; 0, 0];
  predicted := LieGroups.SE23.Quat.exp_mixed(
    previous, left_increment, right_increment, coupling);
end predictSe23EstimatorState;

function predictSe23EstimatorCovariance
  input Real previous[9, 9];
  input Real state_previous[10];
  input Real dt;
  input Real gyro[3];
  input Real specific_force[3];
  output Real predicted[9, 9];
protected
  constant Real position_random_walk_density = 0.0025;
  constant Real accelerometer_noise_density = 0.04;
  constant Real gyro_noise_density = 0.0004;
  Real transition[9, 9];
  Real process_covariance[9, 9];
  Real specific_force_skew[3, 3];
  Real gyro_skew[3, 3];
  Real attitude_velocity_jacobian[3, 3];
  Real rotation_previous[3, 3];
algorithm
  transition := identity(9);
  process_covariance := zeros(9, 9);
  rotation_previous := LieGroups.SO3.Quat.to_DCM(state_previous[7:10]);
  specific_force_skew := [
    0.0, -specific_force[3], specific_force[2];
    specific_force[3], 0.0, -specific_force[1];
    -specific_force[2], specific_force[1], 0.0];
  gyro_skew := [
    0.0, -gyro[3], gyro[2];
    gyro[3], 0.0, -gyro[1];
    -gyro[2], gyro[1], 0.0];
  attitude_velocity_jacobian :=
    (-1.0) * rotation_previous * specific_force_skew;
  for axis in 1:3 loop
    transition[axis, axis + 3] := dt;
    process_covariance[axis, axis] :=
      position_random_walk_density * dt
        + accelerometer_noise_density * dt^3 / 3.0;
    process_covariance[axis, axis + 3] :=
      accelerometer_noise_density * dt^2 / 2.0;
    process_covariance[axis + 3, axis] :=
      process_covariance[axis, axis + 3];
    process_covariance[axis + 3, axis + 3] :=
      accelerometer_noise_density * dt;
    process_covariance[axis + 6, axis + 6] := gyro_noise_density * dt;
    for attitude_axis in 1:3 loop
      transition[axis, attitude_axis + 6] :=
        0.5 * dt^2 * attitude_velocity_jacobian[axis, attitude_axis];
      transition[axis + 3, attitude_axis + 6] :=
        dt * attitude_velocity_jacobian[axis, attitude_axis];
      transition[axis + 6, attitude_axis + 6] :=
        (if axis == attitude_axis then 1.0 else 0.0)
          - dt * gyro_skew[axis, attitude_axis];
    end for;
  end for;
  predicted := transition * previous * transpose(transition)
    + process_covariance;
end predictSe23EstimatorCovariance;

function correctSe23GpsPosition
  input Real state_predicted[10];
  input Real covariance_predicted[9, 9];
  input Real gps_position[3];
  input Real gps_position_variance;
  output Real state_next[10];
  output Real covariance_updated[9, 9];
protected
  Real correction[9];
  Real correction_skew[3, 3];
  Real attitude_correction_quat[4];
  Real innovation_covariance[3, 3];
  Real innovation_inverse[3, 3];
  Real gain[9, 3];
  Real residual[3];
  Real joseph_factor[9, 9];
  Real covariance_reset[9, 9];
algorithm
  innovation_covariance := covariance_predicted[1:3, 1:3];
  for axis in 1:3 loop
    innovation_covariance[axis, axis] :=
      innovation_covariance[axis, axis] + gps_position_variance;
  end for;
  innovation_inverse := symmetricPositiveDefiniteInverse3(
    innovation_covariance);
  gain := covariance_predicted[:, 1:3] * innovation_inverse;
  residual := gps_position - state_predicted[1:3];
  correction := gain * residual;
  joseph_factor := identity(9);
  for row in 1:9 loop
    for col in 1:3 loop
      joseph_factor[row, col] := joseph_factor[row, col] - gain[row, col];
    end for;
  end for;
  for axis in 1:3 loop
    state_next[axis] := state_predicted[axis] + correction[axis];
    state_next[axis + 3] := state_predicted[axis + 3] + correction[axis + 3];
  end for;
  attitude_correction_quat := LieGroups.SO3.Quat.exp_map(correction[7:9]);
  attitude_correction_quat := LieGroups.SO3.Quat.product(
    state_predicted[7:10], attitude_correction_quat);
  for index in 1:4 loop
    state_next[index + 6] := attitude_correction_quat[index];
  end for;
  covariance_updated :=
    joseph_factor * covariance_predicted * transpose(joseph_factor)
      + gps_position_variance * gain * transpose(gain);
  correction_skew := [
    0.0, -correction[9], correction[8];
    correction[9], 0.0, -correction[7];
    -correction[8], correction[7], 0.0];
  covariance_reset := identity(9);
  for row in 1:3 loop
    for col in 1:3 loop
      covariance_reset[row + 6, col + 6] :=
        (if row == col then 1.0 else 0.0)
          - 0.5 * correction_skew[row, col];
    end for;
  end for;
  covariance_updated := covariance_reset * covariance_updated
    * transpose(covariance_reset);
end correctSe23GpsPosition;

function packSe23EstimatorPacket
  input Real state[10];
  input Real covariance[9, 9];
  output Real packet[91];
protected
  Real covariance_stable[9, 9];
  Real normalized_quat[4];
algorithm
  for index in 1:6 loop
    packet[index] := state[index];
  end for;
  normalized_quat := LieGroups.SO3.Quat.normalize(state[7:10]);
  for index in 1:4 loop
    packet[index + 6] := normalized_quat[index];
  end for;
  covariance_stable := 0.5 * (covariance + transpose(covariance));
  for row in 1:9 loop
    covariance_stable[row, row] := max(covariance_stable[row, row], 1.0e-12);
  end for;
  for row in 1:9 loop
    for col in 1:9 loop
      packet[10 + (col - 1) * 9 + row] := covariance_stable[row, col];
    end for;
  end for;
end packSe23EstimatorPacket;

function sampledSe23GpsImuFilter
  input Real previous[91];
  input Real sensor_packet[12]
    "{dt, GPS valid, GPS position[3], gyro[3], specific force[3], GPS variance}";
  output Real next[91];
protected
  Real state_previous[10];
  Real state_predicted[10];
  Real state_next[10];
  Real covariance_previous[9, 9];
  Real covariance_predicted[9, 9];
  Real covariance_updated[9, 9];
algorithm
  state_previous := previous[1:10];
  covariance_previous := unpackSe23EstimatorCovariance(previous);
  state_predicted := predictSe23EstimatorState(
    state_previous, sensor_packet[1], sensor_packet[6:8], sensor_packet[9:11]);
  covariance_predicted := predictSe23EstimatorCovariance(
    covariance_previous,
    state_previous,
    sensor_packet[1],
    sensor_packet[6:8],
    sensor_packet[9:11]);
  if sensor_packet[2] > 0.5 then
    (state_next, covariance_updated) := correctSe23GpsPosition(
      state_predicted,
      covariance_predicted,
      sensor_packet[3:5],
      sensor_packet[12]);
  else
    state_next := state_predicted;
    covariance_updated := covariance_predicted;
  end if;
  next := packSe23EstimatorPacket(state_next, covariance_updated);
end sampledSe23GpsImuFilter;

model ReusableBoosterLanding
  parameter Real auto_launch_time(unit = "s") = -1
    "Automatic launch time for non-interactive regression runs; negative disables";
  parameter Real launch_burn_duration(min = 2, max = 10, unit = "s") = 6
    "Main-engine ascent burn duration";
  parameter Real launch_thrust_fraction(min = 0.45, max = 0.8) = 0.62
    "Ascent thrust fraction";
  parameter Real landing_trigger_speed(min = 2, max = 20, unit = "m/s") = 8
    "Downward speed that activates landing guidance";
  parameter Real landing_duration(min = 12, max = 28, unit = "s") = 18
    "Flatness-planned landing duration";
  parameter Real rcs_authority(min = 0, max = 1) = 1
    "Available RCS authority";
  parameter Real wind_speed(min = 0, max = 30, unit = "m/s") = 5
    "Mean horizontal wind speed";
  parameter Real wind_direction_deg(min = -180, max = 180, unit = "deg") = 35
    "Direction the wind blows toward, measured from world +x toward +y";
  parameter Real wind_speed_variation(min = 0, max = 10, unit = "m/s") = 0
    "Slow mean-wind speed variation amplitude";
  parameter Real wind_direction_variation_deg(min = 0, max = 45, unit = "deg") = 15
    "Slow mean-wind direction variation amplitude";
  parameter Real wind_variation_period(min = 5, max = 120, unit = "s") = 10
    "Slow mean-wind variation period";
  parameter Real dryden_intensity(min = 0, max = 10, unit = "m/s") = 4
    "Deterministic Dryden-shaped gust scale";
  parameter Real wave_heave_amplitude(min = 0, max = 2, unit = "m") = 0.35
    "Drone-ship heave amplitude";
  parameter Real wave_roll_amplitude_deg(min = 0, max = 5, unit = "deg") = 1.2
    "Drone-ship roll amplitude";
  parameter Real wave_pitch_amplitude_deg(min = 0, max = 5, unit = "deg") = 0.8
    "Drone-ship pitch amplitude";
  parameter Real wave_period(min = 4, max = 20, unit = "s") = 8.5
    "Dominant drone-ship wave period";

  input Real launch_command(start = 0) "Latched launch command [0..1]";
  input Real wind_speed_input(start = wind_speed) "Live mean-wind setting [m/s]";
  input Real wind_direction_input(start = wind_direction_deg) "Live mean direction [deg]";
  input Real wind_direction_variation_input(start = wind_direction_variation_deg)
    "Live direction-swing amplitude [deg]";
  input Real dryden_intensity_input(start = dryden_intensity)
    "Live deterministic Dryden-shaped gust scale [m/s]";
  input Real wave_heave_amplitude_input(start = wave_heave_amplitude)
    "Live deck-heave amplitude [m]";
  input Real wave_roll_amplitude_input(start = wave_roll_amplitude_deg)
    "Live deck-roll amplitude [deg]";
  input Real wave_pitch_amplitude_input(start = wave_pitch_amplitude_deg)
    "Live deck-pitch amplitude [deg]";
  input Real position_gain_scale_input(start = 1)
    "Live SE_2(3) position-gain multiplier";
  input Real velocity_gain_scale_input(start = 1)
    "Live SE_2(3) velocity-gain multiplier";
  input Real attitude_gain_scale_input(start = 1)
    "Live attitude-gain multiplier";
  input Real rate_gain_scale_input(start = 1)
    "Live angular-rate-gain multiplier";

  output Real position[3](
    start = {0, 0, cg_to_foot},
    each fixed = true) "Booster CG position [m]";
  output Real velocity[3](
    start = {0, 0, wave_heave_amplitude * 2 * pi / wave_period},
    each fixed = true) "World velocity [m/s]";
  output Real quat[4](start = {1, 0, 0, 0}, each fixed = true)
    "Body-to-world quaternion {w,x,y,z}";
  output Real omega[3](start = {0, 0, 0}, each fixed = true)
    "Body angular velocity [rad/s]";
  output Real vehicle_mass(start = initial_mass, fixed = true) "Vehicle mass [kg]";
  output Real thrust(start = 0, fixed = true) "Center-engine thrust [N]";
  output Real mission_phase
    "0 waiting, 1 ascent, 2 coast, 3 landing, 4 landed, 5 crashed";
  output Real mission_elapsed "Time since launch command [s]";
  output Real landing_elapsed "Time since landing guidance activation [s]";

  output Real reference_position[3] "Flatness reference position [m]";
  output Real reference_velocity[3] "Flatness reference velocity [m/s]";
  output Real reference_acceleration[3] "Flatness reference acceleration [m/s^2]";
  output Real reference_quat[4] "Flatness-derived reference attitude";
  output Real acceleration_command[3] "SE_2(3)-corrected acceleration command [m/s^2]";
  output Real se23_error[9] "SE_2(3) logarithmic tracking error";
  output Real tracking_error "Position tracking error norm [m]";

  output Real estimated_position[3] "GPS/IMU SE_2(3) position estimate [m]";
  output Real estimated_velocity[3] "GPS/IMU SE_2(3) velocity estimate [m/s]";
  output Real estimated_quat[4] "GPS/IMU SE_2(3) attitude estimate";
  output Real estimated_position_variance[3]
    "World-axis estimator position variances [m^2]";
  output Real estimated_attitude_variance[3]
    "Body-axis right-error attitude variances [rad^2]";
  output Real estimated_position_attitude_covariance[3]
    "Representative position-attitude covariance terms";
  output Real estimator_position_error "Position-estimate error norm [m]";
  output Real gps_position_measurement[3] "Synthetic GPS position measurement [m]";
  output Real gps_measurement_variance[3] "GPS position variances [m^2]";
  output Real imu_gyro[3] "Body-rate IMU measurement [rad/s]";
  output Real imu_specific_force[3] "Body-frame IMU specific force [m/s^2]";
  output Real gps_update "GPS correction indicator [0..1]";

  output Real thrust_fraction "Commanded plume intensity [0..1]";
  output Real gimbal_x "Engine force deflection toward body +x [rad]";
  output Real gimbal_y "Engine force deflection toward body +y [rad]";
  output Real rcs_x_positive "RCS body +x actuator fraction [0..1]";
  output Real rcs_x_negative "RCS body -x actuator fraction [0..1]";
  output Real rcs_y_positive "RCS body +y actuator fraction [0..1]";
  output Real rcs_y_negative "RCS body -y actuator fraction [0..1]";
  output Real rcs_roll_positive "RCS body +z actuator fraction [0..1]";
  output Real rcs_roll_negative "RCS body -z actuator fraction [0..1]";
  output Real rcs_operating_fraction "Altitude-scheduled RCS availability [0..1]";
  output Real rcs_propellant_used(start = 0, fixed = true)
    "Integrated RCS propellant consumption [kg]";
  output Real leg_deploy "Landing-leg deployment fraction [0..1]";
  output Real altitude "Engine-plane altitude above deck [m]";
  output Real wind_velocity[3] "World-frame wind velocity [m/s]";
  output Real mean_wind_velocity[3] "Slowly varying mean wind [m/s]";
  output Real dryden_gust_velocity[3] "World-frame Dryden gust velocity [m/s]";
  output Real wind_speed_display "Wind speed for viewer telemetry [m/s]";
  output Real wind_direction_display "Wind direction for viewer telemetry [deg]";
  output Real deck_position[3] "Drone-ship deck-center position [m]";
  output Real deck_velocity[3] "Drone-ship deck-center velocity [m/s]";
  output Real deck_quat[4] "Drone-ship body-to-world quaternion";
  output Real deck_angular_velocity[3] "Drone-ship world angular velocity [rad/s]";
  output Real deck_roll_display "Drone-ship roll [deg]";
  output Real deck_pitch_display "Drone-ship pitch [deg]";
  output Real crash_indicator "Latched destructive-impact indicator [0..1]";
  output Real impact_speed "Surface-relative impact speed [m/s]";
  output Real impact_tilt_deg "Booster tilt from the local surface normal [deg]";
  output Real supporting_legs "Number of meaningfully loaded landing feet";
  output Real support_margin "CG margin inside the loaded-foot support polygon [m]";
  output Real touchdown_normal_speed "Maximum active foot-contact normal speed [m/s]";
  output Real touchdown_tangential_speed "Maximum active foot-contact tangential speed [m/s]";
  output Real touchdown_safe "Touchdown acceptance indicator [0..1]";

  output Real plan_initial_position[3] "Planner initial position [m]";
  output Real plan_initial_velocity[3] "Planner initial velocity [m/s]";
  output Real plan_initial_acceleration[3] "Planner initial acceleration [m/s^2]";
  output Real plan_target_position[3] "Planner target position [m]";
  output Real plan_target_velocity[3] "Planner target velocity [m/s]";
  output Real plan_target_acceleration[3] "Planner target acceleration [m/s^2]";
  output Real plan_target_normal_offset[3]
    "Terminal CG offset from deck center [m]";
  output Real plan_duration "Planner duration [s]";
  output Real reference_feasible "Sampled reference-feasibility indicator [0..1]";

protected
  constant Real pi = 3.141592653589793;
  constant Real gravity = 9.80665 "Standard gravity [m/s^2]";
  constant Real controller_period = 0.05 "Geometric controller update period [s]";
  constant Real estimator_period = 0.10 "GPS/IMU estimator update period [s]";
  constant Real gps_period = 0.20 "GPS measurement period [s]";
  constant Integer gps_decimation = integer(gps_period / estimator_period);
  constant Real gps_position_variance = 0.09 "Differential-GPS variance [m^2]";
  constant Real initial_mass = 34000 "Representative landing mass [kg]";
  constant Real isp = 282 "Center-engine sea-level specific impulse [s]";
  constant Real max_thrust = 845000 "One center-engine sea-level thrust [N]";
  constant Real min_thrust = 0.30 * max_thrust "Demonstration deep-throttle floor [N]";
  constant Real engine_tau = 0.12 "Engine response time constant [s]";
  constant Real booster_length = 40.9 "First-stage length [m]";
  constant Real cg_to_foot = 0.5 * booster_length "CG to deployed foot plane [m]";
  constant Real engine_lever = 18.0 "CG to center-engine thrust point [m]";
  constant Real rcs_lever = 17.0 "CG to upper RCS pods [m]";
  constant Real gimbal_limit = 5.0 * pi / 180.0 "Center-engine gimbal limit [rad]";
  constant Real rcs_force_limit = 6000 "Net lateral RCS authority per axis [N]";
  constant Real rcs_roll_limit = 18000 "RCS axial moment authority [N*m]";
  constant Real rcs_tau = 0.08 "RCS actuator response time constant [s]";
  constant Real inertia[3] = {4800000, 4800000, 60000}
    "Representative principal landing inertias [kg*m^2]";
  constant Real rho = 1.225 "Air density [kg/m^3]";
  constant Real drag_area[3] = {75, 75, 8.5} "Body-axis Cd*A [m^2]";
  constant Real contact_k = 1500000 "Landing-leg stiffness [N/m]";
  constant Real contact_c = 180000 "Landing-leg normal damping [N*s/m]";
  constant Real contact_tangent_c = 40000 "Landing-foot tangential damping [N*s/m]";
  constant Real contact_friction_coefficient = 0.8
    "Regularized landing-foot friction coefficient";
  constant Real minimum_support_load_fraction = 0.02
    "Minimum vehicle-weight fraction carried by each supporting foot";
  constant Real touchdown_settle_time = 0.35
    "Minimum contact-settling interval before touchdown classification [s]";
  constant Real touchdown_settle_timeout = 2.0
    "Maximum interval allowed to establish stable landing support [s]";
  constant Real dryden_reference_speed = 20 "Dryden reference airspeed [m/s]";
  constant Real dryden_length_u = 200 "Longitudinal Dryden scale length [m]";
  constant Real dryden_length_v = 100 "Lateral Dryden scale length [m]";
  constant Real dryden_length_w = 50 "Vertical Dryden scale length [m]";
  Real dryden_u_state(start = 0, fixed = true);
  Real dryden_v_state[2](each start = 0, each fixed = true);
  Real dryden_w_state[2](each start = 0, each fixed = true);
  Real dryden_noise[3];
  Real dryden_local_velocity[3];
  Real mean_wind_speed;
  Real mean_wind_direction;
  Real dryden_tau_u;
  Real dryden_tau_v;
  Real dryden_tau_w;
  Real flat_trajectory[9];
  discrete Real launch_time(start = -1, fixed = true);
  discrete Real landing_start_time(start = -1, fixed = true);
  discrete Real crash_latched(start = 0, fixed = true);
  discrete Real landed_latched(start = 0, fixed = true);
  discrete Real crash_speed_latched(start = 0, fixed = true);
  discrete Real crash_tilt_latched(start = 0, fixed = true);
  discrete Real touchdown_cutoff_latched(start = 0, fixed = true);
  discrete Real contact_start_time(start = -1, fixed = true);
  discrete Real launch_initial_position[3](start = {0, 0, cg_to_foot}, each fixed = true);
  discrete Real launch_initial_velocity[3](
    start = {0, 0, wave_heave_amplitude * 2 * pi / wave_period},
    each fixed = true);
  discrete Real landing_initial_position[3](start = {0, 0, cg_to_foot}, each fixed = true);
  discrete Real landing_initial_velocity[3](each start = 0, each fixed = true);
  discrete Real landing_initial_acceleration[3](
    start = {0, 0, -gravity}, each fixed = true);
  Real launch_reference_acceleration;
  Real coast_elapsed;
  Real burn_end_position[3];
  Real burn_end_velocity[3];
  Real effective_wind_speed;
  Real effective_wind_direction;
  Real effective_wind_direction_variation;
  Real effective_dryden_intensity;
  Real effective_wave_heave;
  Real effective_wave_roll;
  Real effective_wave_pitch;
  Real effective_position_gain;
  Real effective_velocity_gain;
  Real effective_attitude_gain;
  Real effective_rate_gain;
  Real terminal_horizontal_gain;
  Real controller_position_gain_scale[3];
  Real controller_velocity_gain_scale[3];
  Real mission_control_gate;
  discrete Real controller_command[27](each start = 0, each fixed = true);
  discrete Real estimator_packet[91](
    start = initialSe23EstimatorPacket(
      {0.6, -0.4, cg_to_foot + 0.3},
      {0, 0, wave_heave_amplitude * 2 * pi / wave_period}),
    each fixed = true);
  discrete Integer estimator_sample(start = 0, fixed = true);
  discrete Real plan_feasible_state(start = 1, fixed = true);
  Real rotation[3, 3];
  Real air_velocity_w[3];
  Real body_velocity[3];
  Real drag_b[3];
  Real drag_w[3];
  Real moment_command[3];
  Real thrust_request;
  Real thrust_command;
  Real lateral_force_request[2];
  Real lateral_force_scale;
  Real lateral_force_limit;
  Real engine_force_b[3];
  Real engine_moment_b[3];
  Real moment_residual[3];
  Real rcs_force_command_b[3];
  Real rcs_roll_moment_command;
  Real rcs_force_b[3](each start = 0, each fixed = true);
  Real rcs_roll_moment(start = 0, fixed = true);
  Real rcs_moment_b[3];
  Real rcs_mass_flow;
  Real total_force_b[3];
  Real total_moment_b[3];
  Real quaternion_derivative[4];
  Real quaternion_norm_error;
  Real angular_momentum[3];
  Real deck_roll;
  Real deck_pitch;
  Real deck_roll_rate;
  Real deck_pitch_rate;
  Real deck_rotation[3, 3];
  Real deck_normal_w[3];
  Real engine_plane_position_w[3];
  Real engine_plane_position_deck[3];
  Real engine_plane_velocity_w[3];
  Real engine_plane_relative_velocity_w[3];
  Real body_up_surface_cosine;
  Real surface_relative_speed;
  Real surface_normal_speed;
  Real surface_tangential_speed;
  Real surface_tilt_deg;
  Real surface_contact;
  Real touchdown_evaluation_ready;
  Real touchdown_settle_expired;
  Real deck_target[12];

  Real leg_r_b[3, 4] "Deployed landing-foot offsets in body coordinates [m]";
  Real leg_position_w[3, 4];
  Real leg_velocity_w[3, 4];
  Real leg_position_deck[3, 4];
  Real deck_velocity_at_leg_w[3, 4];
  Real leg_relative_velocity_w[3, 4];
  Real leg_tangent_velocity_w[3, 4];
  Real leg_normal_speed[4];
  Real leg_normal_force[4];
  Real leg_loaded[4];
  Real leg_tangential_speed[4];
  Real leg_on_deck[4];
  Real leg_force_w[3, 4];
  Real leg_force_b[3, 4];
  Real leg_compression[4];
  Real leg_contact_gate[4];
  Real contact_force_w[3];
  Real contact_moment_b[3];
  Real cg_position_deck[3];

equation
  leg_r_b = [
    -4.5, 4.5, 0, 0;
    0, 0, -4.5, 4.5;
    -cg_to_foot, -cg_to_foot, -cg_to_foot, -cg_to_foot];

  effective_wind_speed = max(0.0, wind_speed_input);
  effective_wind_direction = wind_direction_input;
  effective_wind_direction_variation = max(0.0, wind_direction_variation_input);
  effective_dryden_intensity = max(0.0, dryden_intensity_input);
  effective_wave_heave = max(0.0, wave_heave_amplitude_input);
  effective_wave_roll = max(0.0, wave_roll_amplitude_input);
  effective_wave_pitch = max(0.0, wave_pitch_amplitude_input);
  effective_position_gain = min(max(position_gain_scale_input, 0.0), 3.0);
  effective_velocity_gain = min(max(velocity_gain_scale_input, 0.0), 3.0);
  effective_attitude_gain = min(max(attitude_gain_scale_input, 0.0), 3.0);
  effective_rate_gain = min(max(rate_gain_scale_input, 0.0), 3.0);

  when (launch_command > 0.5
      or (auto_launch_time >= 0 and time >= auto_launch_time))
      and pre(launch_time) < 0 then
    launch_time = time;
    launch_initial_position = position;
    launch_initial_velocity = velocity;
  end when;
  when launch_time >= 0
      and time >= launch_time + launch_burn_duration
      and velocity[3] <= -landing_trigger_speed
      and pre(landing_start_time) < 0 then
    landing_start_time = time;
    landing_initial_position = position;
    landing_initial_velocity = velocity;
    landing_initial_acceleration = {
      der(velocity[1]),
      der(velocity[2]),
      max(der(velocity[3]), min_thrust / vehicle_mass - gravity)};
  end when;

  mission_elapsed = if launch_time >= 0.0 then time - launch_time else 0.0;
  landing_elapsed = if landing_start_time >= 0.0 then
    time - landing_start_time else 0.0;
  terminal_horizontal_gain = if landing_start_time >= 0 then
    min(max((landing_duration - landing_elapsed) / 4.0, 0.0), 1.0) else 1.0;
  controller_position_gain_scale = effective_position_gain
    * {terminal_horizontal_gain, terminal_horizontal_gain, 1.0};
  controller_velocity_gain_scale = effective_velocity_gain * {1, 1, 1};
  mission_phase = if crash_latched > 0.5 then 5
    else if landed_latched > 0.5 then 4
    else if launch_time < 0 then 0
    else if time < launch_time + launch_burn_duration then 1
    else if landing_start_time < 0 then 2
    else 3;
  mission_control_gate = if mission_phase >= 1 and mission_phase <= 3
    and crash_latched < 0.5 then 1.0 else 0.0;
  rcs_operating_fraction = scheduledRcsAvailability(
    altitude, mission_control_gate);
  launch_reference_acceleration = launch_thrust_fraction * max_thrust / initial_mass - gravity;
  coast_elapsed = max(mission_elapsed - launch_burn_duration, 0.0);
  burn_end_position = {
    launch_initial_position[1]
      + launch_burn_duration * launch_initial_velocity[1],
    launch_initial_position[2]
      + launch_burn_duration * launch_initial_velocity[2],
    launch_initial_position[3]
      + launch_burn_duration * launch_initial_velocity[3]
      + 0.5 * launch_reference_acceleration * launch_burn_duration^2};
  burn_end_velocity = {
    launch_initial_velocity[1],
    launch_initial_velocity[2],
    launch_initial_velocity[3]
      + launch_reference_acceleration * launch_burn_duration};

  plan_initial_position = landing_initial_position;
  plan_initial_velocity = landing_initial_velocity;
  plan_initial_acceleration = landing_initial_acceleration;
  deck_target = movingDeckCgTarget(
    max(landing_start_time, 0.0) + landing_duration,
    effective_wave_heave,
    effective_wave_roll * pi / 180.0,
    effective_wave_pitch * pi / 180.0,
    wave_period,
    cg_to_foot);
  plan_target_position = {deck_target[1], deck_target[2], deck_target[3]};
  plan_target_velocity = {deck_target[4], deck_target[5], deck_target[6]};
  plan_target_acceleration = {deck_target[7], deck_target[8], deck_target[9]};
  plan_target_normal_offset = cg_to_foot
    * {deck_target[10], deck_target[11], deck_target[12]};
  plan_duration = landing_duration;
  when sample(0.0, 0.5) then
    plan_feasible_state = if pre(landing_start_time) < 0.0 then 1.0 else
      quinticPlanFeasible(
        cat(1, plan_initial_position, plan_initial_velocity, plan_initial_acceleration),
        cat(1, plan_target_position, plan_target_velocity, plan_target_acceleration),
        {
          plan_duration,
          vehicle_mass,
          min_thrust,
          max_thrust,
          cg_to_foot,
          max(vehicle_mass - 27000.0, 0.0)});
  end when;
  reference_feasible = plan_feasible_state;

  mean_wind_speed = max(
    0.0,
    effective_wind_speed
      + wind_speed_variation * sin(2 * pi * time / wind_variation_period));
  mean_wind_direction = (
    effective_wind_direction
      + effective_wind_direction_variation
        * sin(2 * pi * time / wind_variation_period + 0.7))
    * pi / 180.0;
  mean_wind_velocity = {
    mean_wind_speed * cos(mean_wind_direction),
    mean_wind_speed * sin(mean_wind_direction),
    0};
  dryden_tau_u = dryden_length_u / dryden_reference_speed;
  dryden_tau_v = dryden_length_v / dryden_reference_speed;
  dryden_tau_w = dryden_length_w / dryden_reference_speed;
  dryden_noise = {
    (sin(0.17 * time + 0.2) + sin(0.43 * time + 1.7) + sin(0.91 * time + 2.6)) / sqrt(3.0),
    (sin(0.23 * time + 2.0) + sin(0.61 * time + 0.4) + sin(1.13 * time + 1.1)) / sqrt(3.0),
    (sin(0.31 * time + 1.3) + sin(0.79 * time + 2.4) + sin(1.47 * time + 0.1)) / sqrt(3.0)};
  der(dryden_u_state) = (dryden_noise[1] - dryden_u_state) / dryden_tau_u;
  der(dryden_v_state[1]) = dryden_v_state[2];
  der(dryden_v_state[2]) = (
    dryden_noise[2] - dryden_v_state[1]
      - 2 * dryden_tau_v * dryden_v_state[2]) / (dryden_tau_v * dryden_tau_v);
  der(dryden_w_state[1]) = dryden_w_state[2];
  der(dryden_w_state[2]) = (
    dryden_noise[3] - dryden_w_state[1]
      - 2 * dryden_tau_w * dryden_w_state[2]) / (dryden_tau_w * dryden_tau_w);
  dryden_local_velocity = {
    effective_dryden_intensity * sqrt(2 * dryden_length_u / (pi * dryden_reference_speed))
      * dryden_u_state,
    effective_dryden_intensity * sqrt(dryden_length_v / (pi * dryden_reference_speed))
      * (dryden_v_state[1] + sqrt(3.0) * dryden_tau_v * dryden_v_state[2]),
    effective_dryden_intensity * sqrt(dryden_length_w / (pi * dryden_reference_speed))
      * (dryden_w_state[1] + sqrt(3.0) * dryden_tau_w * dryden_w_state[2])};
  dryden_gust_velocity = {
    cos(mean_wind_direction) * dryden_local_velocity[1]
      - sin(mean_wind_direction) * dryden_local_velocity[2],
    sin(mean_wind_direction) * dryden_local_velocity[1]
      + cos(mean_wind_direction) * dryden_local_velocity[2],
    dryden_local_velocity[3]};
  wind_velocity = mean_wind_velocity + dryden_gust_velocity;
  wind_speed_display = sqrt(wind_velocity[1]^2 + wind_velocity[2]^2);
  wind_direction_display = atan2(wind_velocity[2], wind_velocity[1]) * 180.0 / pi;

  deck_roll = effective_wave_roll * pi / 180.0
    * sin(2 * pi * time / wave_period);
  deck_pitch = effective_wave_pitch * pi / 180.0
    * sin(1.66 * pi * time / wave_period);
  deck_roll_rate = effective_wave_roll * pi / 180.0 * 2 * pi / wave_period
    * cos(2 * pi * time / wave_period);
  deck_pitch_rate = effective_wave_pitch * pi / 180.0 * 1.66 * pi / wave_period
    * cos(1.66 * pi * time / wave_period);
  deck_position = {
    0,
    0,
    effective_wave_heave * sin(2 * pi * time / wave_period)};
  deck_velocity = {
    0,
    0,
    effective_wave_heave * 2 * pi / wave_period * cos(2 * pi * time / wave_period)};
  deck_quat = {
    cos(0.5 * deck_pitch) * cos(0.5 * deck_roll),
    cos(0.5 * deck_pitch) * sin(0.5 * deck_roll),
    sin(0.5 * deck_pitch) * cos(0.5 * deck_roll),
    -sin(0.5 * deck_pitch) * sin(0.5 * deck_roll)};
  deck_angular_velocity = {
    deck_roll_rate * cos(deck_pitch),
    deck_pitch_rate,
    -deck_roll_rate * sin(deck_pitch)};
  deck_roll_display = deck_roll * 180.0 / pi;
  deck_pitch_display = deck_pitch * 180.0 / pi;
  deck_rotation = LieGroups.SO3.Quat.to_DCM(deck_quat);
  deck_normal_w = deck_rotation * {0, 0, 1};

  engine_plane_position_w = position + rotation * {0, 0, -cg_to_foot};
  engine_plane_position_deck = transpose(deck_rotation)
    * (engine_plane_position_w - deck_position);
  engine_plane_velocity_w = velocity
    + rotation * cross(omega, {0, 0, -cg_to_foot});
  engine_plane_relative_velocity_w = engine_plane_velocity_w
    - deck_velocity
    - cross(deck_angular_velocity, engine_plane_position_w - deck_position);
  surface_relative_speed = sqrt(
    engine_plane_relative_velocity_w * engine_plane_relative_velocity_w);
  surface_normal_speed = deck_normal_w * engine_plane_relative_velocity_w;
  surface_tangential_speed = sqrt(max(
    surface_relative_speed^2 - surface_normal_speed^2,
    0.0));
  body_up_surface_cosine = min(max(
    rotation[:, 3] * deck_normal_w,
    -1.0), 1.0);
  surface_tilt_deg = acos(body_up_surface_cosine) * 180.0 / pi;
  surface_contact = if (
      abs(engine_plane_position_deck[1]) < 44
        and abs(engine_plane_position_deck[2]) < 25
        and engine_plane_position_deck[3] <= 0.15)
      or (leg_deploy > 0.5 and (
        leg_on_deck[1] * leg_compression[1] > 1.0e-4
          or leg_on_deck[2] * leg_compression[2] > 1.0e-4
          or leg_on_deck[3] * leg_compression[3] > 1.0e-4
          or leg_on_deck[4] * leg_compression[4] > 1.0e-4))
      or engine_plane_position_w[3] <= -2.2 then 1.0 else 0.0;
  when landing_start_time >= 0 and surface_contact > 0.5
      and pre(contact_start_time) < 0 then
    contact_start_time = time;
  end when;
  when landing_start_time >= 0 and surface_contact > 0.5
      and touchdownKinematicsSafe(
        touchdown_normal_speed,
        touchdown_tangential_speed,
        surface_tilt_deg) > 0.5
      and pre(touchdown_cutoff_latched) < 0.5 then
    touchdown_cutoff_latched = 1.0;
  end when;
  touchdown_evaluation_ready = if contact_start_time >= 0
    and time >= contact_start_time + touchdown_settle_time then 1.0 else 0.0;
  touchdown_settle_expired = if contact_start_time >= 0
    and time >= contact_start_time + touchdown_settle_timeout then 1.0 else 0.0;
  when landing_start_time >= 0 and pre(crash_latched) < 0.5
      and pre(landed_latched) < 0.5
      and touchdown_evaluation_ready > 0.5
      and touchdown_safe < 0.5
      and (touchdownKinematicsSafe(
        touchdown_normal_speed,
        touchdown_tangential_speed,
        surface_tilt_deg) < 0.5 or touchdown_settle_expired > 0.5) then
    crash_latched = 1.0;
    crash_speed_latched = surface_relative_speed;
    crash_tilt_latched = surface_tilt_deg;
  end when;
  when landing_start_time >= 0 and pre(landed_latched) < 0.5
      and pre(crash_latched) < 0.5
      and touchdown_evaluation_ready > 0.5
      and touchdown_safe > 0.5 then
    landed_latched = 1.0;
  end when;
  crash_indicator = crash_latched;
  impact_speed = if crash_latched > 0.5 then crash_speed_latched
    else surface_relative_speed;
  impact_tilt_deg = if crash_latched > 0.5 then crash_tilt_latched
    else surface_tilt_deg;

  estimated_position = {
    estimator_packet[1], estimator_packet[2], estimator_packet[3]};
  estimated_velocity = {
    estimator_packet[4], estimator_packet[5], estimator_packet[6]};
  estimated_quat = {
    estimator_packet[7], estimator_packet[8],
    estimator_packet[9], estimator_packet[10]};
  estimated_position_variance = {
    estimator_packet[11], estimator_packet[21], estimator_packet[31]};
  estimated_attitude_variance = {
    estimator_packet[71], estimator_packet[81], estimator_packet[91]};
  estimated_position_attitude_covariance = {
    estimator_packet[74], estimator_packet[66], estimator_packet[85]};
  gps_measurement_variance = {
    gps_position_variance, gps_position_variance, gps_position_variance};
  gps_update = if mod(estimator_sample - 1, gps_decimation) == 0 then 1.0 else 0.0;
  gps_position_measurement = {
    position[1] + 0.30 * sin(0.71 * time + 0.30),
    position[2] + 0.30 * sin(0.53 * time + 1.10),
    position[3] + 0.20 * sin(0.61 * time + 2.00)};
  imu_gyro = {
    omega[1] + 0.0005 * sin(17.1 * time + 0.2),
    omega[2] + 0.0005 * sin(15.7 * time + 1.4),
    omega[3] + 0.0005 * sin(13.9 * time + 2.1)};

  flat_trajectory = quinticFlatnessReference(
    {landing_elapsed, landing_duration},
    cat(1, plan_initial_position, plan_initial_velocity, plan_initial_acceleration),
    cat(1, plan_target_position, plan_target_velocity, plan_target_acceleration));
  reference_position = if mission_phase < 0.5 then
      {0, 0, deck_position[3] + cg_to_foot}
    else if mission_phase < 1.5 then
      {
        launch_initial_position[1] + mission_elapsed * launch_initial_velocity[1],
        launch_initial_position[2] + mission_elapsed * launch_initial_velocity[2],
        launch_initial_position[3] + mission_elapsed * launch_initial_velocity[3]
          + 0.5 * launch_reference_acceleration * mission_elapsed^2}
    else if mission_phase < 2.5 then
      {
        burn_end_position[1] + coast_elapsed * burn_end_velocity[1],
        burn_end_position[2] + coast_elapsed * burn_end_velocity[2],
        burn_end_position[3] + coast_elapsed * burn_end_velocity[3]
          - 0.5 * gravity * coast_elapsed^2}
    else if mission_phase < 3.5 then
      {flat_trajectory[1], flat_trajectory[2], flat_trajectory[3]}
    else
      {0, 0, deck_position[3] + cg_to_foot};
  reference_velocity = if mission_phase < 0.5 then
      deck_velocity
    else if mission_phase < 1.5 then
      {launch_initial_velocity[1], launch_initial_velocity[2],
        launch_initial_velocity[3] + launch_reference_acceleration * mission_elapsed}
    else if mission_phase < 2.5 then
      {burn_end_velocity[1], burn_end_velocity[2],
        burn_end_velocity[3] - gravity * coast_elapsed}
    else if mission_phase < 3.5 then
      {flat_trajectory[4], flat_trajectory[5], flat_trajectory[6]}
    else
      deck_velocity;
  reference_acceleration = if mission_phase < 0.5 or mission_phase >= 3.5 then
      {0, 0, -effective_wave_heave * (2 * pi / wave_period)^2
        * sin(2 * pi * time / wave_period)}
    else if mission_phase < 1.5 then
      {0, 0, launch_reference_acceleration}
    else if mission_phase < 2.5 then
      {0, 0, -gravity + 0.05}
    else
      {flat_trajectory[7], flat_trajectory[8], flat_trajectory[9]};

  reference_quat = controller_command[1:4];
  se23_error = controller_command[5:13];
  acceleration_command = controller_command[14:16];
  moment_command = controller_command[20:22];
  thrust_request = controller_command[23];
  rotation = LieGroups.SO3.Quat.to_DCM(quat);
  air_velocity_w = velocity - wind_velocity;
  body_velocity = transpose(rotation) * air_velocity_w;

  drag_b = {
    -0.5 * rho * drag_area[1] * abs(body_velocity[1]) * body_velocity[1],
    -0.5 * rho * drag_area[2] * abs(body_velocity[2]) * body_velocity[2],
      -0.5 * rho * drag_area[3] * abs(body_velocity[3]) * body_velocity[3]};
  drag_w = rotation * drag_b;
  imu_specific_force = (
    total_force_b + transpose(rotation) * contact_force_w) / vehicle_mass + {
      0.01 * sin(11.3 * time + 0.5),
      0.01 * sin(12.7 * time + 1.7),
      0.01 * sin(10.1 * time + 2.8)};
  when sample(0.0, estimator_period) then
    estimator_packet = sampledSe23GpsImuFilter(
      pre(estimator_packet),
      cat(
        1,
        {
          if pre(estimator_sample) == 0 then 0.0 else estimator_period,
          if mod(pre(estimator_sample), gps_decimation) == 0 then 1.0 else 0.0},
        gps_position_measurement,
        pre(imu_gyro),
        pre(imu_specific_force),
        {gps_position_variance}));
    estimator_sample = pre(estimator_sample) + 1;
  end when;
  when sample(0.0, controller_period) then
    controller_command = sampledSe23Controller(
      cat(
        1,
        estimator_packet[1:10],
        imu_gyro,
        {
          pre(controller_command[24]), pre(controller_command[25]),
          pre(controller_command[26]), pre(controller_command[27])}),
      cat(1, reference_position, reference_velocity, reference_acceleration),
      cat(
        1,
        controller_position_gain_scale,
        controller_velocity_gain_scale,
        {effective_attitude_gain, effective_rate_gain}),
      cat(1, {controller_period, vehicle_mass}, inertia, drag_w));
  end when;
  thrust_command = if mission_phase >= 0.5 and mission_phase < 1.5 then
      launch_thrust_fraction * max_thrust
    else if mission_phase >= 2.5 and mission_phase < 3.5
        and touchdown_cutoff_latched < 0.5 then
      min(max(thrust_request, min_thrust), max_thrust)
    else 0.0;
  der(thrust) = (thrust_command - thrust) / engine_tau;

  lateral_force_request = {
    -moment_command[2] / engine_lever,
    moment_command[1] / engine_lever};
  lateral_force_limit = thrust * sin(gimbal_limit);
  lateral_force_scale = min(
    1.0,
    lateral_force_limit / sqrt(
      lateral_force_request[1]^2 + lateral_force_request[2]^2 + 1.0));
  engine_force_b = {
    lateral_force_scale * lateral_force_request[1],
    lateral_force_scale * lateral_force_request[2],
    sqrt(max(
      thrust^2
        - (lateral_force_scale * lateral_force_request[1])^2
        - (lateral_force_scale * lateral_force_request[2])^2,
      0.0))};
  engine_moment_b = {
    engine_lever * engine_force_b[2],
    -engine_lever * engine_force_b[1],
    0};
  gimbal_x = atan2(engine_force_b[1], max(engine_force_b[3], 1.0));
  gimbal_y = atan2(engine_force_b[2], max(engine_force_b[3], 1.0));

  moment_residual = moment_command - engine_moment_b;
  rcs_force_command_b = {
    min(max(moment_residual[2] / rcs_lever, -rcs_operating_fraction * rcs_authority * rcs_force_limit), rcs_operating_fraction * rcs_authority * rcs_force_limit),
    min(max(-moment_residual[1] / rcs_lever, -rcs_operating_fraction * rcs_authority * rcs_force_limit), rcs_operating_fraction * rcs_authority * rcs_force_limit),
    0};
  rcs_roll_moment_command = min(max(
    moment_residual[3],
    -rcs_operating_fraction * rcs_authority * rcs_roll_limit),
    rcs_operating_fraction * rcs_authority * rcs_roll_limit);
  der(rcs_force_b) = (rcs_force_command_b - rcs_force_b) / rcs_tau;
  der(rcs_roll_moment) =
    (rcs_roll_moment_command - rcs_roll_moment) / rcs_tau;
  rcs_moment_b = {
    -rcs_lever * rcs_force_b[2],
    rcs_lever * rcs_force_b[1],
    rcs_roll_moment};
  rcs_mass_flow = rcsPropellantMassFlow(rcs_force_b, rcs_roll_moment);
  der(rcs_propellant_used) = rcs_mass_flow;
  der(vehicle_mass) = -thrust / (isp * gravity) - rcs_mass_flow;

  rcs_x_positive = max(rcs_force_b[1], 0.0) / rcs_force_limit;
  rcs_x_negative = max(-rcs_force_b[1], 0.0) / rcs_force_limit;
  rcs_y_positive = max(rcs_force_b[2], 0.0) / rcs_force_limit;
  rcs_y_negative = max(-rcs_force_b[2], 0.0) / rcs_force_limit;
  rcs_roll_positive = max(rcs_moment_b[3], 0.0) / rcs_roll_limit;
  rcs_roll_negative = max(-rcs_moment_b[3], 0.0) / rcs_roll_limit;

  leg_deploy = if mission_phase < 0.5 then 1.0
    else if mission_phase < 1.5 then max(0.0, 1.0 - mission_elapsed / 1.0)
    else if mission_phase < 2.5 then 0.0
    else if mission_phase < 3.5 then
      min(max((landing_elapsed - (landing_duration - 4.0)) / 1.5, 0.0), 1.0)
    else 1.0;
  for leg in 1:4 loop
    leg_position_w[:, leg] = position + rotation * leg_r_b[:, leg];
    leg_velocity_w[:, leg] = velocity + rotation * cross(omega, leg_r_b[:, leg]);
    leg_position_deck[:, leg] = transpose(deck_rotation) * (leg_position_w[:, leg] - deck_position);
    leg_on_deck[leg] = if abs(leg_position_deck[1, leg]) < 44
      and abs(leg_position_deck[2, leg]) < 25 then 1.0 else 0.0;
    deck_velocity_at_leg_w[:, leg] = deck_velocity
      + cross(deck_angular_velocity, leg_position_w[:, leg] - deck_position);
    leg_relative_velocity_w[:, leg] = leg_velocity_w[:, leg] - deck_velocity_at_leg_w[:, leg];
    leg_normal_speed[leg] =
      deck_normal_w[1] * leg_relative_velocity_w[1, leg]
      + deck_normal_w[2] * leg_relative_velocity_w[2, leg]
      + deck_normal_w[3] * leg_relative_velocity_w[3, leg];
    leg_tangent_velocity_w[:, leg] = leg_relative_velocity_w[:, leg]
      - leg_normal_speed[leg] * deck_normal_w;
    leg_compression[leg] = max(0.0, -leg_position_deck[3, leg]);
    leg_contact_gate[leg] = leg_deploy * leg_on_deck[leg]
      * min(max(leg_compression[leg] / 0.05, 0.0), 1.0);
    leg_normal_force[leg] = leg_contact_gate[leg] * max(
      0.0,
      contact_k * leg_compression[leg] - contact_c * leg_normal_speed[leg]);
    leg_loaded[leg] = if leg_normal_force[leg]
      >= minimum_support_load_fraction * vehicle_mass * gravity then 1.0 else 0.0;
    leg_tangential_speed[leg] = sqrt(
      leg_tangent_velocity_w[1, leg]^2
        + leg_tangent_velocity_w[2, leg]^2
        + leg_tangent_velocity_w[3, leg]^2);
    leg_force_w[:, leg] = leg_normal_force[leg] * deck_normal_w
      + tangentialContactForce(
        leg_normal_force[leg],
        leg_contact_gate[leg],
        leg_tangent_velocity_w[:, leg],
        contact_tangent_c,
        contact_friction_coefficient);
    leg_force_b[:, leg] = transpose(rotation) * leg_force_w[:, leg];
  end for;
  contact_force_w = {
    sum(leg_force_w[axis, foot] for foot in 1:4) for axis in 1:3};
  contact_moment_b =
      cross(leg_r_b[:, 1], leg_force_b[:, 1])
    + cross(leg_r_b[:, 2], leg_force_b[:, 2])
    + cross(leg_r_b[:, 3], leg_force_b[:, 3])
    + cross(leg_r_b[:, 4], leg_force_b[:, 4]);
  cg_position_deck = transpose(deck_rotation) * (position - deck_position);
  supporting_legs = sum(leg_loaded);
  support_margin = loadedFootSupportMargin(
    leg_position_deck,
    {cg_position_deck[1], cg_position_deck[2]},
    leg_loaded);
  touchdown_normal_speed = loadedFootMaximum(
    {abs(leg_normal_speed[foot]) for foot in 1:4},
    leg_loaded,
    leg_contact_gate,
    abs(surface_normal_speed));
  touchdown_tangential_speed = loadedFootMaximum(
    leg_tangential_speed,
    leg_loaded,
    leg_contact_gate,
    surface_tangential_speed);
  touchdown_safe = touchdownAcceptance(
    supporting_legs,
    support_margin,
    touchdown_normal_speed,
    touchdown_tangential_speed,
    surface_tilt_deg);

  total_force_b = engine_force_b + rcs_force_b + drag_b;
  total_moment_b = engine_moment_b + rcs_moment_b + contact_moment_b;
  der(position) = velocity;
  der(velocity) = (rotation * total_force_b + contact_force_w) / vehicle_mass + {0, 0, -gravity};
  quaternion_norm_error = quat[1]^2 + quat[2]^2 + quat[3]^2 + quat[4]^2 - 1.0;
  quaternion_derivative = LieGroups.SO3.Quat.kinematics(quat, omega);
  der(quat) = quaternion_derivative - quaternion_norm_error * quat;
  angular_momentum = inertia .* omega;
  der(omega) = (total_moment_b - cross(omega, angular_momentum)) ./ inertia;

  thrust_fraction = min(max(thrust / max_thrust, 0.0), 1.0);
  tracking_error = sqrt(
    (position - reference_position) * (position - reference_position));
  estimator_position_error = sqrt(
    (estimated_position - position) * (estimated_position - position));
  altitude = deck_normal_w * (engine_plane_position_w - deck_position);
end ReusableBoosterLanding;

model ReusableBoosterEmbeddedControlLaw
  constant Real samplePeriod = 0.05 "Controller sample period [s]";
  input Real translation[10]
    "{reference acceleration[3], world correction[3], drag[3], mass}";
  input Real rotation[9] "{attitude error[3], omega[3], reference omega[3]}";
  input Real gains[2] "{attitude gain, rate gain}";
  input Real inertia[3] "Principal moments of inertia [kg*m^2]";
  discrete output Real control[9](each start = 0.0, each fixed = true)
    "{acceleration[3], force[3], moment[3]}";
equation
  when sample(0.0, samplePeriod) then
    control = reusableBoosterEmbeddedControlLaw(
      translation, rotation, gains, inertia);
  end when;
end ReusableBoosterEmbeddedControlLaw;
