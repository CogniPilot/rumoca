// Physical humanoid: a free-floating MultiBody robot that stands on compliant foot contact.
//
// Nothing is driven kinematically. The pelvis is a free body (6 DOF); every joint is a
// revolute driven only by a torque from its own joint controller (PD toward a pose); each
// foot touches the ground through four sole-corner Hunt-Crossley contacts with regularized
// Coulomb friction (GroundContact.mo). The contact law is smooth and event-free.
//
// Proportions are those of a generic full-size electric humanoid: about 1.5 m tall and
// 89 kg, with segment masses and inertias from box and rod approximations. Every joint
// torque passes through an actuator limit (peak torque, derated with joint speed). Every
// body touches the ground: rounded sole corners, capsule legs and arms, a rounded-box
// torso and head, spheres for the pelvis and hands. Each body's contacts are summed into one
// force and torque by a GroundContactBody, so the robot can kneel, land on its hands, roll
// and fall.
//
// Temporary constraint (rumoca issue i-81cfe659): simulate this model at the top level.
// When a MultiBody model that declares `inner world` is instantiated as a component of
// another model, rumoca currently drops gravity, so variants are made by changing the
// parameters of this model, not by wrapping it.

model HumanoidPhysical
  parameter Real l_thigh = 0.40 "Thigh length [m]";
  parameter Real l_shin = 0.40 "Shin length [m]";
  parameter Real ankle_h = 0.07 "Ankle height above sole [m]";
  parameter Real hip_w = 0.10 "Hip half width [m]";
  parameter Real l_upper_arm = 0.27;
  parameter Real l_forearm = 0.25;
  // Gripper hands: a finger group and an opposed thumb, each on a torque-driven hinge.
  parameter Real l_finger = 0.06 "Finger length [m]";
  parameter Real l_thumb = 0.045 "Thumb length [m]";
  parameter Real tau_finger_max = 6 "Finger actuator peak torque [N.m]";
  parameter Real w_finger_max = 12 "Finger no-load speed [rad/s]";
  parameter Real kp_finger = 5;
  parameter Real kd_finger = 0.05;
  parameter Real grip_period = 4 "Demo grip cycle: open, close, open [s]";
  parameter Real shoulder_w = 0.20;
  parameter Real shoulder_h = 0.42;
  parameter Real l_neck = 0.18 "Shoulder line to head centre [m]";
  // Standing pose: knees bent, foot flat under the hip. The bend leaves the reach for steps:
  // hip to ankle is 0.74 m of the 0.80 m leg, so a 0.3 m step stays reachable.
  parameter Real q_hip0 = -0.40 "Hip pitch [rad]";
  parameter Real q_knee0 = 0.80 "Knee [rad]";
  parameter Real q_ankle0 = -0.40 "Ankle pitch [rad]";
  final parameter Real z_pelvis0 = l_thigh * cos(q_hip0) + l_shin * cos(q_hip0 + q_knee0) + ankle_h + 0.002 "Start 2 mm above the floor";
  // Joint PD gains.
  parameter Real kp_leg = 1300 "[N.m/rad]";
  parameter Real kd_leg = 50 "[N.m.s/rad]";
  // Capture-point ankle strategy: ankle torque pushes the capture point back toward the
  // centre of the feet, smoothly saturated near the foot-tipping limit.
  parameter Real k_cp = 970 "Ankle torque per metre of capture-point offset [N.m/m]";
  parameter Real tau_ankle_max = 65 "Ankle balance torque limit [N.m]";
  parameter Real z_com = 0.80 "Nominal CoM height for the capture point [m]";
  final parameter Real omega_cp = sqrt(9.81 / z_com);
  // Stepping: when the capture point leaves the comfortable support region, step to where it
  // will be at touchdown, x_cp(T) = p + (x_cp - p) exp(omega T), measured from the stance foot.
  // The target is re-planned continuously with the swing time left, so a push that is still
  // acting when the step starts moves the target with it.
  parameter Real T_step = 0.25 "Swing duration [s]";
  parameter Real h_swing = 0.06 "Swing foot lift [m]";
  parameter Real cp_margin = 0.02 "A step starts when the capture point is this close to leaving the feet [m]";
  parameter Real step_margin = 0.03 "Foot placed this far beyond the predicted capture point [m]";
  parameter Real step_max = 0.30 "Longest step from the stance foot, fore-aft [m]";
  parameter Real step_side_max = 0.45 "Widest step from the stance foot, sideways [m]";
  parameter Real f_touchdown = 200 "Swing foot load that ends the swing early [N]";
  parameter Real z_fallen = 0.55 "Pelvis height below which the robot has fallen and stops stepping [m]";
  parameter Real foot_c = 0.055 "Foot centre ahead of the ankle [m]";
  parameter Real tau_centre = 0.6 "Time constant of the pelvis re-centring in double support [s]";
  parameter Real walk_lead = 0.25 "How far the desired pelvis leads the feet per unit walking speed [s]";
  parameter Real T_ds = 0.08 "Double support between walking steps [s]";
  parameter Real walk_on = 0.05 "Commanded speed above which the robot walks with timed steps [m/s]";
  parameter Real k_path = 0.9 "Step correction toward the commanded path, per step [1]";
  Real walk_cmd = if time >= 1 and time < 3 then 0.3 else 0;
  Real side_cmd = 0;
  Boolean walking = abs(walk_cmd) > walk_on or abs(side_cmd) > walk_on "Timed stepping toward the commanded path";
  Real path[2](each start = 0, each fixed = true) "Point moving at the commanded velocity that the walk follows, x y [m]";
  Real L_walk[2] "Step displacement for the next step: commanded travel plus path correction [m]";
  parameter Real kp_arm = 150;
  parameter Real kd_arm = 8;
  // Actuator limits: peak torque [N.m] and no-load joint speed [rad/s]. Available torque falls
  // linearly from the peak at rest to zero at the no-load speed.
  parameter Real tau_hip_roll_max = 300, tau_hip_pitch_max = 400, tau_knee_max = 450;
  parameter Real tau_ankle_pitch_max = 250, tau_ankle_roll_max = 150;
  parameter Real tau_shoulder_max = 150, tau_elbow_max = 100;
  parameter Real w_leg_max = 20 "Leg joint no-load speed [rad/s]";
  parameter Real w_arm_max = 25 "Arm joint no-load speed [rad/s]";
  // Contact.
  parameter Real k_c = 1e5 "Contact stiffness per corner [N/m]";
  parameter Real d_c = 0.5 "Hunt-Crossley damping [s/m]";
  parameter Real mu = 0.8 "Friction coefficient";
  parameter Real eps = 1e-4 "Penetration smoothing [m]";
  parameter Real v_eps = 0.01 "Friction regularization speed [m/s]";
  // Disturbance: a smooth horizontal push on the torso, peak push_peak at t = push_t.
  parameter Real push_peak = 0 "Peak push force, +x [N]";
  parameter Real push_t = 1.0 "Push centre time [s]";
  parameter Real push_w = 0.04 "Push width (Gaussian sigma) [s]";
  parameter Real r_sole = 0.01 "Sole corner rounding [m]";
  parameter Real sole[4, 3] = {{0.18, 0.055, r_sole - ankle_h}, {0.18, -0.055, r_sole - ankle_h}, {-0.07, 0.055, r_sole - ankle_h}, {-0.07, -0.055, r_sole - ankle_h}} "Sole corner sphere centres in the foot frame [m]";
  parameter Real torso_half[3] = {0.11, 0.18, shoulder_h / 2} "Torso box half extents [m]";
  parameter Real r_torso = 0.04 "Torso box edge rounding [m]";

  inner MultiBodyLie.World world(n = {0, 0, -1}, enableAnimation = false);

  MultiBodyLie.Parts.Body pelvis(m = 12, I_11 = 0.12, I_22 = 0.10, I_33 = 0.12, r_CM = {0, 0, 0},
    r_0(start = {0, 0, z_pelvis0}, each fixed = true), v_0(each fixed = true),
    angles_fixed = true, w_0_fixed = true, animation = false);
  MultiBodyLie.Parts.BodyShape torso(r = {0, 0, shoulder_h}, r_CM = {0, 0, shoulder_h / 2}, m = 30, I_11 = 0.56, I_22 = 0.77, I_33 = 0.44, animation = false);
  MultiBodyLie.Parts.BodyShape neck(r = {0, 0, l_neck}, r_CM = {0, 0, l_neck}, m = 5, I_11 = 0.03, I_22 = 0.03, I_33 = 0.03, animation = false);

  MultiBodyLie.Parts.FixedTranslation hipOffset[2](r = {{0, hip_w, 0}, {0, -hip_w, 0}}, each animation = false);
  MultiBodyLie.Joints.Revolute hipRoll[2](each n = {1, 0, 0}, each useAxisFlange = true, each animation = false, each phi(start = 0, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Joints.Revolute hipPitch[2](each n = {0, 1, 0}, each useAxisFlange = true, each animation = false, each phi(start = q_hip0, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Parts.BodyShape thigh[2](each r = {0, 0, -l_thigh}, each r_CM = {0, 0, -l_thigh / 2}, each m = 8, each I_11 = 0.11, each I_22 = 0.11, each I_33 = 0.02, each animation = false);
  MultiBodyLie.Joints.Revolute knee[2](each n = {0, 1, 0}, each useAxisFlange = true, each animation = false, each phi(start = q_knee0, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Parts.BodyShape shin[2](each r = {0, 0, -l_shin}, each r_CM = {0, 0, -l_shin / 2}, each m = 5, each I_11 = 0.067, each I_22 = 0.067, each I_33 = 0.008, each animation = false);
  MultiBodyLie.Joints.Revolute anklePitch[2](each n = {0, 1, 0}, each useAxisFlange = true, each animation = false, each phi(start = q_ankle0, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Joints.Revolute ankleRoll[2](each n = {1, 0, 0}, each useAxisFlange = true, each animation = false, each phi(start = 0, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Parts.BodyShape foot[2](each r = {0.17, 0, -ankle_h}, each r_CM = {0.05, 0, -ankle_h / 2}, each m = 1.5, each I_11 = 0.003, each I_22 = 0.008, each I_33 = 0.008, each animation = false);

  MultiBodyLie.Parts.FixedTranslation shoulderOffset[2](r = {{0, shoulder_w, 0}, {0, -shoulder_w, 0}}, each animation = false);
  MultiBodyLie.Joints.Revolute shoulder[2](each n = {0, 1, 0}, each useAxisFlange = true, each animation = false, each phi(start = 0, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Parts.BodyShape upperArm[2](each r = {0, 0, -l_upper_arm}, each r_CM = {0, 0, -l_upper_arm / 2}, each m = 3.5, each I_11 = 0.021, each I_22 = 0.021, each I_33 = 0.004, each animation = false);
  MultiBodyLie.Joints.Revolute elbow[2](each n = {0, 1, 0}, each useAxisFlange = true, each animation = false, each phi(start = -0.35, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Parts.BodyShape forearm[2](each r = {0, 0, -l_forearm}, each r_CM = {0, 0, -l_forearm / 2}, each m = 2.5, each I_11 = 0.013, each I_22 = 0.013, each I_33 = 0.002, each animation = false);

  Real push_x = 0 "Operator push on the torso, +x [N]";
  Real push_y = 0 "Operator push on the torso, +y [N]";
  MultiBodyLie.Forces.WorldForce push(resolveInFrame = MultiBodyLie.Types.ResolveInFrameB.world, animation = false);

  // Joint torque actuators.
  Modelica.Mechanics.Rotational.Sources.Torque tauHipRoll[2], tauHipPitch[2], tauKnee[2], tauAnklePitch[2], tauAnkleRoll[2], tauShoulder[2], tauElbow[2];

  // Ground contact, one element per body. Capsules are their two end spheres; the torso is a
  // rounded box (eight corner spheres).
  GroundContactBody footC[2](each n = 4, each centre = sole, each radius = fill(r_sole, 4), each k = k_c, each d = d_c, each mu = mu, each eps = eps, each v_eps = v_eps);
  GroundContactBody shinC[2](each n = 2, each centre = {{0, 0, 0}, {0, 0, -l_shin}}, each radius = {0.06, 0.05}, each k = k_c, each d = d_c, each mu = mu, each eps = eps, each v_eps = v_eps);
  GroundContactBody thighC[2](each n = 2, each centre = {{0, 0, 0}, {0, 0, -l_thigh}}, each radius = {0.07, 0.065}, each k = k_c, each d = d_c, each mu = mu, each eps = eps, each v_eps = v_eps);
  GroundContactBody upperArmC[2](each n = 2, each centre = {{0, 0, 0}, {0, 0, -l_upper_arm}}, each radius = {0.06, 0.05}, each k = k_c, each d = d_c, each mu = mu, each eps = eps, each v_eps = v_eps);
  // Hands. Positive rotation about the hand's left axis swings a hanging finger tip backwards,
  // toward the thumb, so the finger closes with positive angle and the thumb with negative.
  MultiBodyLie.Parts.FixedTranslation fingerBase[2](each r = {0.02, 0, -0.03}, each animation = false);
  MultiBodyLie.Parts.FixedTranslation thumbBase[2](each r = {-0.025, 0, -0.02}, each animation = false);
  MultiBodyLie.Joints.Revolute finger[2](each n = {0, 1, 0}, each useAxisFlange = true, each animation = false,
    each phi(start = -0.35, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Joints.Revolute thumb[2](each n = {0, 1, 0}, each useAxisFlange = true, each animation = false,
    each phi(start = 0.4, fixed = true), each w(start = 0, fixed = true));
  MultiBodyLie.Parts.BodyShape fingerBody[2](each r = {0, 0, -l_finger}, each r_CM = {0, 0, -l_finger / 2}, each m = 0.12,
    each I_11 = 4e-5, each I_22 = 4e-5, each I_33 = 1e-5, each animation = false);
  MultiBodyLie.Parts.BodyShape thumbBody[2](each r = {0, 0, -l_thumb}, each r_CM = {0, 0, -l_thumb / 2}, each m = 0.08,
    each I_11 = 2e-5, each I_22 = 2e-5, each I_33 = 5e-6, each animation = false);
  Modelica.Mechanics.Rotational.Sources.Torque tauFinger[2], tauThumb[2];
  GroundContactBody fingerC[2](each n = 1, each centre = {{0, 0, -l_finger}}, each radius = {0.012}, each k = k_c, each d = d_c, each mu = mu, each eps = eps, each v_eps = v_eps);
  GroundContactBody thumbC[2](each n = 1, each centre = {{0, 0, -l_thumb}}, each radius = {0.012}, each k = k_c, each d = d_c, each mu = mu, each eps = eps, each v_eps = v_eps);
  Real grip "Grip command, 0 open to 1 closed";
  output Real fingerL = finger[1].phi, fingerR = finger[2].phi, thumbL = thumb[1].phi, thumbR = thumb[2].phi;
  GroundContactBody forearmC[2](each n = 1, each centre = {{0, 0, -l_forearm}}, each radius = {0.05}, each k = k_c, each d = d_c, each mu = mu, each eps = eps, each v_eps = v_eps);
  GroundContactBody torsoC(n = 8, centre = {{(torso_half[1] - r_torso), (torso_half[2] - r_torso), torso_half[3] + (torso_half[3] - r_torso)}, {(torso_half[1] - r_torso), (torso_half[2] - r_torso), torso_half[3] - (torso_half[3] - r_torso)}, {(torso_half[1] - r_torso), -(torso_half[2] - r_torso), torso_half[3] + (torso_half[3] - r_torso)}, {(torso_half[1] - r_torso), -(torso_half[2] - r_torso), torso_half[3] - (torso_half[3] - r_torso)}, {-(torso_half[1] - r_torso), (torso_half[2] - r_torso), torso_half[3] + (torso_half[3] - r_torso)}, {-(torso_half[1] - r_torso), (torso_half[2] - r_torso), torso_half[3] - (torso_half[3] - r_torso)}, {-(torso_half[1] - r_torso), -(torso_half[2] - r_torso), torso_half[3] + (torso_half[3] - r_torso)}, {-(torso_half[1] - r_torso), -(torso_half[2] - r_torso), torso_half[3] - (torso_half[3] - r_torso)}}, radius = fill(r_torso, 8), k = k_c, d = d_c, mu = mu, eps = eps, v_eps = v_eps);
  GroundContactBody pelvisC(n = 1, centre = {{0, 0, 0}}, radius = {0.12}, k = k_c, d = d_c, mu = mu, eps = eps, v_eps = v_eps);
  // Square head: a 0.14 x 0.2 x 0.2 m block with 6 cm rounded edges.
  GroundContactBody headC(n = 8, centre = {{0.01, 0.04, l_neck + 0.04}, {0.01, 0.04, l_neck - 0.04}, {0.01, -0.04, l_neck + 0.04}, {0.01, -0.04, l_neck - 0.04}, {-0.01, 0.04, l_neck + 0.04}, {-0.01, 0.04, l_neck - 0.04}, {-0.01, -0.04, l_neck + 0.04}, {-0.01, -0.04, l_neck - 0.04}}, radius = fill(0.06, 8), k = k_c, d = d_c, mu = mu, eps = eps, v_eps = v_eps);

  // Gait state. phase: 0 double support, 1 left foot swinging, 2 right foot swinging.
  discrete Integer phase(start = 0, fixed = true);
  discrete Integer lastSwing(start = 2, fixed = true);
  discrete Integer steps(start = 0, fixed = true);
  discrete Real t_step0(start = 0, fixed = true) "Start of the current swing [s]";
  discrete Real t_touchdown(start = -1, fixed = true) "Time of the last touchdown [s]";
  discrete Real footL[2](start = {0, hip_w}, each fixed = true) "Planted left ankle, x y [m]";
  discrete Real footR[2](start = {0, -hip_w}, each fixed = true) "Planted right ankle, x y [m]";
  discrete Real swingFrom[2](each start = 0, each fixed = true);
  Real swingTo[2] "Swing ankle target, re-planned continuously from the capture point [m]";
  Real pd[2](each start = 0, each fixed = true) "Desired pelvis position, x y [m]";
  Real s_sw "Swing progress 0..1";
  Real swing[3] "Swing ankle target, world [m]";
  Real ankleDesL[3], ankleDesR[3] "Ankle targets, world [m]";
  Real qL[5], qR[5] "Leg joint targets: hip roll, hip pitch, knee, ankle pitch, ankle roll [rad]";
  Real cp_excess "How far the capture point lies outside the support of both feet, less the margin [m]";
  // Event indicators, each live only in the gait phase it belongs to, so a relation crosses once per
  // step instead of chattering on a stance foot's force or height (every crossing is a solver event).
  Real stepNeed "cp_excess in double support, negative while a foot swings [m]";
  Real swingLoad "Normal force on the swinging foot, 0 in double support [N]";
  Real swingHeight "Height of the swinging ankle, high in double support [m]";
  Real x_cp "Capture point, x [m]";
  Real y_cp "Capture point, y [m]";
  Real x_feet "Centre of the feet, x [m]";
  Real y_feet "Centre of the feet, y [m]";
  Real tau_bal_pitch "Balance torque added at each ankle pitch [N.m]";
  Real tau_bal_roll "Balance torque added at each ankle roll [N.m]";
  output Real pelvis_x = pelvis.r_0[1], pelvis_y = pelvis.r_0[2], pelvis_z = pelvis.r_0[3];
  // Frame points for the viewer, read from MultiBody (world coordinates).
  output Real hipL_x = hipRoll[1].frame_a.r_0[1], hipL_y = hipRoll[1].frame_a.r_0[2], hipL_z = hipRoll[1].frame_a.r_0[3];
  output Real kneeL_x = knee[1].frame_a.r_0[1], kneeL_y = knee[1].frame_a.r_0[2], kneeL_z = knee[1].frame_a.r_0[3];
  output Real ankleL_x = anklePitch[1].frame_a.r_0[1], ankleL_y = anklePitch[1].frame_a.r_0[2], ankleL_z = anklePitch[1].frame_a.r_0[3];
  output Real toeL_x = foot[1].frame_b.r_0[1], toeL_y = foot[1].frame_b.r_0[2], toeL_z = foot[1].frame_b.r_0[3];
  output Real hipR_x = hipRoll[2].frame_a.r_0[1], hipR_y = hipRoll[2].frame_a.r_0[2], hipR_z = hipRoll[2].frame_a.r_0[3];
  output Real kneeR_x = knee[2].frame_a.r_0[1], kneeR_y = knee[2].frame_a.r_0[2], kneeR_z = knee[2].frame_a.r_0[3];
  output Real ankleR_x = anklePitch[2].frame_a.r_0[1], ankleR_y = anklePitch[2].frame_a.r_0[2], ankleR_z = anklePitch[2].frame_a.r_0[3];
  output Real toeR_x = foot[2].frame_b.r_0[1], toeR_y = foot[2].frame_b.r_0[2], toeR_z = foot[2].frame_b.r_0[3];
  output Real shoulderL_x = shoulder[1].frame_a.r_0[1], shoulderL_y = shoulder[1].frame_a.r_0[2], shoulderL_z = shoulder[1].frame_a.r_0[3];
  output Real elbowL_x = elbow[1].frame_a.r_0[1], elbowL_y = elbow[1].frame_a.r_0[2], elbowL_z = elbow[1].frame_a.r_0[3];
  output Real handL_x = forearm[1].frame_b.r_0[1], handL_y = forearm[1].frame_b.r_0[2], handL_z = forearm[1].frame_b.r_0[3];
  output Real shoulderR_x = shoulder[2].frame_a.r_0[1], shoulderR_y = shoulder[2].frame_a.r_0[2], shoulderR_z = shoulder[2].frame_a.r_0[3];
  output Real elbowR_x = elbow[2].frame_a.r_0[1], elbowR_y = elbow[2].frame_a.r_0[2], elbowR_z = elbow[2].frame_a.r_0[3];
  output Real handR_x = forearm[2].frame_b.r_0[1], handR_y = forearm[2].frame_b.r_0[2], handR_z = forearm[2].frame_b.r_0[3];
  output Real head_x = neck.frame_b.r_0[1], head_y = neck.frame_b.r_0[2], head_z = neck.frame_b.r_0[3];
  output Real px = x_feet - 0.06, py = y_feet, xi_x = x_cp, xi_y = y_cp "Feet centre and capture point, for the floor rings";
  output Real push_fx = push.force[1] "Total push on the torso (scripted plus operator), +x [N]";
  output Real push_fy = push.force[2] "Total push on the torso, +y [N]";
  output Real grf_left = footC[1].fz "Left foot normal force [N]";
  output Real grf_right = footC[2].fz "Right foot normal force [N]";
  output Real f_body = shinC[1].fz + shinC[2].fz + thighC[1].fz + thighC[2].fz + upperArmC[1].fz + upperArmC[2].fz
    + forearmC[1].fz + forearmC[2].fz + fingerC[1].fz + fingerC[2].fz + thumbC[1].fz + thumbC[2].fz + torsoC.fz + pelvisC.fz + headC.fz "Ground force on everything but the feet [N]";
  output Real kneeTorqueL = tauKnee[1].tau, kneeTorqueR = tauKnee[2].tau;
equation
  connect(pelvis.frame_a, torso.frame_a);
  grip = 0.5 - 0.5 * cos(2 * Modelica.Constants.pi * time / grip_period);
  // Swing target from the capture point now and the swing time left.
  // Walking places each foot so the capture point keeps moving at the commanded speed and
  // sways between the feet; otherwise the step brings the capture point to rest.
  // The path point moves at the commanded velocity while walking and rests on the pelvis
  // otherwise; each step travels the commanded distance plus a share of the path error, so
  // the walk neither drifts sideways nor runs ahead of the command.
  der(path) = if walking then {walk_cmd, side_cmd} else (pelvis.r_0[1:2] - path) / 0.2;
  L_walk = {walk_cmd, side_cmd} * (T_step + T_ds) + k_path * (path - pelvis.r_0[1:2]);
  swingTo = if phase == 1 then (if walking
                                then walkTarget(footR, {x_cp, y_cp}, 1, omega_cp, max(T_step - (time - t_step0), 0), T_step + T_ds, foot_c, L_walk, 2 * hip_w, step_max, step_side_max)
                                else stepTarget(footR, {x_cp, y_cp}, 1, omega_cp, max(T_step - (time - t_step0), 0), foot_c, step_margin, step_max, step_side_max, 2 * hip_w))
            elseif phase == 2 then (if walking
                                    then walkTarget(footL, {x_cp, y_cp}, -1, omega_cp, max(T_step - (time - t_step0), 0), T_step + T_ds, foot_c, L_walk, 2 * hip_w, step_max, step_side_max)
                                    else stepTarget(footL, {x_cp, y_cp}, -1, omega_cp, max(T_step - (time - t_step0), 0), foot_c, step_margin, step_max, step_side_max, 2 * hip_w))
            else {0, 0};
  // Swing trajectory: smooth blend in x y, a sine arc in z.
  s_sw = min(max((time - t_step0) / T_step, 0), 1);
  swing = {swingFrom[1] + (swingTo[1] - swingFrom[1]) * s_sw ^ 2 * (3 - 2 * s_sw),
           swingFrom[2] + (swingTo[2] - swingFrom[2]) * s_sw ^ 2 * (3 - 2 * s_sw),
           ankle_h + h_swing * sin(Modelica.Constants.pi * s_sw)};
  ankleDesL = if phase == 1 then swing else {footL[1], footL[2], ankle_h};
  ankleDesR = if phase == 2 then swing else {footR[1], footR[2], ankle_h};
  // Desired pelvis: re-centre over the feet in double support; follow the body while stepping.
  // Walking: the desired pelvis leads the centre of the feet by walk_lead times the commanded
  // speed, so the body leans ahead of its support, the capture point leaves the feet and the
  // capture-point stepping takes the next step. Walking emerges from balance recovery.
  der(pd) = if phase == 0 then ({(footL[1] + footR[1]) / 2 + walk_lead * walk_cmd, (footL[2] + footR[2]) / 2 + walk_lead * side_cmd} - pd) / tau_centre
            else (pelvis.r_0[1:2] - pd) / 0.02;
  // Joint targets from leg inverse kinematics, hip (pelvis upright at standing height) to ankle.
  // A stance leg aims for the pose that holds the pelvis upright over its planted foot, which is
  // what rights the torso. A swing leg tracks its world-frame trajectory from the actual hip
  // pose, so a tilting body does not drive the swing foot into the floor.
  qL = if phase == 1 then legIK(MultiBodyLie.Frames.resolve2(pelvis.frame_a.R, ankleDesL - hipRoll[1].frame_a.r_0), l_thigh, l_shin)
       else legIK({ankleDesL[1] - pd[1], ankleDesL[2] - pd[2] - hip_w, ankleDesL[3] - z_pelvis0}, l_thigh, l_shin);
  qR = if phase == 2 then legIK(MultiBodyLie.Frames.resolve2(pelvis.frame_a.R, ankleDesR - hipRoll[2].frame_a.r_0), l_thigh, l_shin)
       else legIK({ankleDesR[1] - pd[1], ankleDesR[2] - pd[2] + hip_w, ankleDesR[3] - z_pelvis0}, l_thigh, l_shin);
  // Capture point from the pelvis state; the centre of the feet from the ankle frames.
  x_cp = pelvis.r_0[1] + pelvis.v_0[1] / omega_cp;
  y_cp = pelvis.r_0[2] + pelvis.v_0[2] / omega_cp;
  // Capturability: the ankles can hold the capture point anywhere over the feet.
  cp_excess = supportExcess(pre(footL), pre(footR), {x_cp, y_cp}, sole[3, 1] + cp_margin, sole[1, 1] - cp_margin, sole[1, 2] - cp_margin);
  stepNeed = if pre(phase) == 0 then cp_excess else -1;
  swingLoad = if pre(phase) == 1 then footC[1].fz elseif pre(phase) == 2 then footC[2].fz else 0;
  swingHeight = if pre(phase) == 1 then anklePitch[1].frame_a.r_0[3] elseif pre(phase) == 2 then anklePitch[2].frame_a.r_0[3] else 1;
  // Support reference from the gait state before the current event (the gait algorithm reads it).
  x_feet = (if pre(phase) == 1 then pre(footR[1]) elseif pre(phase) == 2 then pre(footL[1]) else (pre(footL[1]) + pre(footR[1])) / 2) + foot_c;
  y_feet = if pre(phase) == 1 then pre(footR[2]) elseif pre(phase) == 2 then pre(footL[2]) else (pre(footL[2]) + pre(footR[2])) / 2;
  // Toe-down (positive pitch) torque moves the body back; pressing the +y edge (negative roll) moves it toward -y.
  // Walking moves the balance reference ahead of the feet, so the ankles stop holding the
  // capture point back and the stepping carries the body forward.
  tau_bal_pitch = tau_ankle_max * tanh(k_cp * (x_cp - x_feet - walk_lead * walk_cmd) / tau_ankle_max);
  // Sideways walking leads the reference the same way, or the ankles would hold every side step back.
  tau_bal_roll = -tau_ankle_max * tanh(k_cp * (y_cp - y_feet - walk_lead * side_cmd) / tau_ankle_max);
  connect(torso.frame_b, neck.frame_a);
  connect(torso.frame_b, push.frame_b);
  connect(torso.frame_a, torsoC.frame_a);
  torsoC.v0 = torso.v_0;
  connect(pelvis.frame_a, pelvisC.frame_a);
  pelvisC.v0 = pelvis.v_0;
  connect(neck.frame_a, headC.frame_a);
  headC.v0 = neck.v_0;
  push.force = {push_peak * exp(-((time - push_t) / push_w) ^ 2) + push_x, push_y, 0};
  for i in 1:2 loop
    connect(pelvis.frame_a, hipOffset[i].frame_a);
    connect(hipOffset[i].frame_b, hipRoll[i].frame_a);
    connect(hipRoll[i].frame_b, hipPitch[i].frame_a);
    connect(hipPitch[i].frame_b, thigh[i].frame_a);
    connect(thigh[i].frame_b, knee[i].frame_a);
    connect(knee[i].frame_b, shin[i].frame_a);
    connect(shin[i].frame_b, anklePitch[i].frame_a);
    connect(anklePitch[i].frame_b, ankleRoll[i].frame_a);
    connect(ankleRoll[i].frame_b, foot[i].frame_a);
    connect(torso.frame_b, shoulderOffset[i].frame_a);
    connect(shoulderOffset[i].frame_b, shoulder[i].frame_a);
    connect(shoulder[i].frame_b, upperArm[i].frame_a);
    connect(upperArm[i].frame_b, elbow[i].frame_a);
    connect(elbow[i].frame_b, forearm[i].frame_a);
    connect(tauHipRoll[i].flange, hipRoll[i].axis);
    connect(tauHipPitch[i].flange, hipPitch[i].axis);
    connect(tauKnee[i].flange, knee[i].axis);
    connect(tauAnklePitch[i].flange, anklePitch[i].axis);
    connect(tauAnkleRoll[i].flange, ankleRoll[i].axis);
    connect(tauShoulder[i].flange, shoulder[i].axis);
    connect(tauElbow[i].flange, elbow[i].axis);
    // Joint controllers: PD toward the inverse-kinematics targets, through the actuator limits.
    // The capture-point balance torque acts only on stance ankles.
    tauHipRoll[i].tau = actuatorLimit(kp_leg * ((if i == 1 then qL[1] else qR[1]) - hipRoll[i].phi) - kd_leg * hipRoll[i].w, hipRoll[i].w, tau_hip_roll_max, w_leg_max);
    tauHipPitch[i].tau = actuatorLimit(kp_leg * ((if i == 1 then qL[2] else qR[2]) - hipPitch[i].phi) - kd_leg * hipPitch[i].w, hipPitch[i].w, tau_hip_pitch_max, w_leg_max);
    tauKnee[i].tau = actuatorLimit(kp_leg * ((if i == 1 then qL[3] else qR[3]) - knee[i].phi) - kd_leg * knee[i].w, knee[i].w, tau_knee_max, w_leg_max);
    tauAnklePitch[i].tau = actuatorLimit(kp_leg * ((if i == 1 then qL[4] else qR[4]) - anklePitch[i].phi) - kd_leg * anklePitch[i].w
      + (if phase == i then 0 else tau_bal_pitch), anklePitch[i].w, tau_ankle_pitch_max, w_leg_max);
    tauAnkleRoll[i].tau = actuatorLimit(kp_leg * ((if i == 1 then qL[5] else qR[5]) - ankleRoll[i].phi) - kd_leg * ankleRoll[i].w
      + (if phase == i then 0 else tau_bal_roll), ankleRoll[i].w, tau_ankle_roll_max, w_leg_max);
    tauShoulder[i].tau = actuatorLimit(kp_arm * (0 - shoulder[i].phi) - kd_arm * shoulder[i].w, shoulder[i].w, tau_shoulder_max, w_arm_max);
    tauElbow[i].tau = actuatorLimit(kp_arm * (-0.35 - elbow[i].phi) - kd_arm * elbow[i].w, elbow[i].w, tau_elbow_max, w_arm_max);
    connect(foot[i].frame_a, footC[i].frame_a);
    footC[i].v0 = foot[i].v_0;
    connect(shin[i].frame_a, shinC[i].frame_a);
    shinC[i].v0 = shin[i].v_0;
    connect(thigh[i].frame_a, thighC[i].frame_a);
    thighC[i].v0 = thigh[i].v_0;
    connect(upperArm[i].frame_a, upperArmC[i].frame_a);
    upperArmC[i].v0 = upperArm[i].v_0;
    connect(forearm[i].frame_b, fingerBase[i].frame_a);
    connect(fingerBase[i].frame_b, finger[i].frame_a);
    connect(finger[i].frame_b, fingerBody[i].frame_a);
    connect(forearm[i].frame_b, thumbBase[i].frame_a);
    connect(thumbBase[i].frame_b, thumb[i].frame_a);
    connect(thumb[i].frame_b, thumbBody[i].frame_a);
    connect(tauFinger[i].flange, finger[i].axis);
    connect(tauThumb[i].flange, thumb[i].axis);
    tauFinger[i].tau = actuatorLimit(kp_finger * (-0.35 + 1.1 * grip - finger[i].phi) - kd_finger * finger[i].w, finger[i].w, tau_finger_max, w_finger_max);
    tauThumb[i].tau = actuatorLimit(kp_finger * (0.4 - 1.0 * grip - thumb[i].phi) - kd_finger * thumb[i].w, thumb[i].w, tau_finger_max, w_finger_max);
    connect(fingerBody[i].frame_a, fingerC[i].frame_a);
    fingerC[i].v0 = fingerBody[i].v_0;
    connect(thumbBody[i].frame_a, thumbC[i].frame_a);
    thumbC[i].v0 = thumbBody[i].v_0;
    connect(forearm[i].frame_a, forearmC[i].frame_a);
    forearmC[i].v0 = forearm[i].v_0;
  end for;
algorithm
  // One when/elsewhen owns the gait state; every branch assigns all of it.
  // Step start: the capture point has left the support region in double support.
  // Touchdown: the swing time is over and the foot is down, or the swing foot has taken
  // load past mid-swing, or the swing has run twice its time; plant the swing foot where it is.
  when pre(phase) == 0 and pelvis.r_0[3] > z_fallen and (stepNeed > 0 or walking and time - pre(t_touchdown) > T_ds) then
    (phase, lastSwing, swingFrom) := stepPlan(pre(footL), pre(footR), {x_cp, y_cp}, pre(lastSwing), time - pre(t_touchdown) < 0.3 or walking);
    t_step0 := time;
    t_touchdown := pre(t_touchdown);
    footL := pre(footL);
    footR := pre(footR);
    steps := pre(steps);
  elsewhen pre(phase) > 0 and (time - pre(t_step0) >= 2 * T_step
      or time - pre(t_step0) >= T_step and swingHeight < ankle_h + 0.015
      or time - pre(t_step0) >= 0.5 * T_step and swingLoad > f_touchdown) then
    footL := if pre(phase) == 1 then {anklePitch[1].frame_a.r_0[1], anklePitch[1].frame_a.r_0[2]} else pre(footL);
    footR := if pre(phase) == 2 then {anklePitch[2].frame_a.r_0[1], anklePitch[2].frame_a.r_0[2]} else pre(footR);
    phase := 0;
    lastSwing := pre(lastSwing);
    swingFrom := pre(swingFrom);
    t_step0 := pre(t_step0);
    t_touchdown := time;
    steps := pre(steps) + 1;
  end when;
end HumanoidPhysical;

function legIK "Joint angles that put the ankle at d from the hip (pelvis frame, pelvis upright), foot flat"
  input Real d[3] "Ankle relative to hip [m]";
  input Real T "Thigh length [m]";
  input Real S "Shin length [m]";
  output Real q[5] "Hip roll, hip pitch, knee, ankle pitch, ankle roll [rad]";
protected
  Real h "Leg length in the roll plane [m]";
  Real L "Hip-to-ankle distance [m]";
algorithm
  // Positive hip roll swings the foot toward +y; positive pitch swings it toward -x.
  q[1] := atan2(d[2], -d[3]);
  h := sqrt(d[2] ^ 2 + d[3] ^ 2);
  L := min(sqrt(d[1] ^ 2 + h ^ 2), 0.999 * (T + S));
  q[3] := Modelica.Constants.pi - acos((T ^ 2 + S ^ 2 - L ^ 2) / (2 * T * S));
  q[2] := -(atan2(d[1], h) + acos((T ^ 2 + L ^ 2 - S ^ 2) / (2 * T * L)));
  q[4] := -(q[2] + q[3]);
  q[5] := -q[1];
end legIK;

function supportExcess "Distance of a point outside the box spanned by both feet (negative inside) [m]"
  input Real footL[2] "Left ankle [m]";
  input Real footR[2] "Right ankle [m]";
  input Real p[2];
  input Real heel "Heel edge ahead of the ankle (negative behind) [m]";
  input Real toe "Toe edge ahead of the ankle [m]";
  input Real half_width "Foot half width [m]";
  output Real excess;
algorithm
  excess := max(max(min(footL[1], footR[1]) + heel - p[1], p[1] - max(footL[1], footR[1]) - toe),
                max(min(footL[2], footR[2]) - half_width - p[2], p[2] - max(footL[2], footR[2]) - half_width));
end supportExcess;

function stepPlan "Choose the swing foot for a capture-point step"
  input Real footL[2] "Planted left ankle [m]";
  input Real footR[2] "Planted right ankle [m]";
  input Real cp[2] "Capture point [m]";
  input Integer lastSwing;
  input Boolean continuing "The previous step landed moments ago";
  output Integer phase "1 swing left, 2 swing right";
  output Integer swingLeg "Same as phase, kept as the last swing";
  output Real from[2] "Swing ankle start [m]";
algorithm
  // A step that follows straight on from the last one swings the other foot, as in walking:
  // lifting the foot that just landed would leave the body over the far foot. The first step
  // from rest uses the foot on the capture point's side, so it also catches lateral motion.
  if continuing or abs(cp[2] - (footL[2] + footR[2]) / 2) <= 0.02 then
    phase := if lastSwing == 1 then 2 else 1;
  else
    phase := if cp[2] > (footL[2] + footR[2]) / 2 then 1 else 2;
  end if;
  swingLeg := phase;
  from := if phase == 1 then footL else footR;
end stepPlan;

function stepTarget "Swing ankle target: the capture point predicted at touchdown, kept clear of the stance foot"
  input Real stance[2] "Stance ankle, x y [m]";
  input Real cp[2] "Capture point now, x y [m]";
  input Integer side "+1 swinging the left foot, -1 the right";
  input Real omega;
  input Real T "Swing time left [s]";
  input Real foot_c "Foot centre ahead of the ankle [m]";
  input Real margin;
  input Real step_max "Longest fore-aft step [m]";
  input Real side_max "Widest sideways step [m]";
  input Real width "Nominal stance width [m]";
  output Real target[2];
protected
  Real c[2] "Predicted capture point relative to the stance foot centre [m]";
  Real n;
algorithm
  c := (cp - {stance[1] + foot_c, stance[2]}) * exp(omega * T);
  // Land so the capture point ends midway between the feet, inside the new support, rather
  // than at its edge where the next instant would trigger another step.
  n := max(sqrt(c[1] ^ 2 + c[2] ^ 2), 1e-6);
  c := c * (2 + margin / n);
  // Fore-aft and sideways reach are limited separately, so a long step never pulls the foot
  // back under the body; the swing foot stays on its own side, most of a stance width away.
  c[1] := min(max(c[1], -step_max), step_max);
  c[2] := if side > 0 then min(max(c[2], 0.75 * width), side_max) else max(min(c[2], -0.75 * width), -side_max);
  target := {stance[1] + c[1], stance[2] + c[2]};
end stepTarget;

function walkTarget "Swing ankle target for steady walking: divergent-component-of-motion offsets"
  input Real stance[2] "Stance ankle, x y [m]";
  input Real cp[2] "Capture point now, x y [m]";
  input Integer side "+1 swinging the left foot, -1 the right";
  input Real omega;
  input Real T "Swing time left [s]";
  input Real T_cycle "Step period, swing plus double support [s]";
  input Real foot_c "Foot centre ahead of the ankle [m]";
  input Real L[2] "Step displacement, x y [m]";
  input Real W "Stance width [m]";
  input Real step_max "Longest fore-aft step [m]";
  input Real side_max "Widest sideways step [m]";
  output Real target[2];
protected
  Real xi[2] "Capture point predicted at touchdown [m]";
  Real e;
algorithm
  // Over one step the capture point moves away from the stance foot as exp(omega t). Placing
  // the next foot L / (e - 1) short of the predicted capture point carries it on by L per step;
  // placing it W / (e + 1) outside makes the sideways motion a steady sway between the feet.
  xi := {stance[1] + foot_c, stance[2]} + (cp - {stance[1] + foot_c, stance[2]}) * exp(omega * T);
  e := exp(omega * T_cycle);
  target := {xi[1] - L[1] / (e - 1) - foot_c, xi[2] - L[2] / (e - 1) + side * W / (e + 1)};
  target[1] := min(max(target[1], stance[1] - step_max), stance[1] + step_max);
  target[2] := if side > 0 then min(max(target[2], stance[2] + 0.5 * W), stance[2] + side_max)
               else max(min(target[2], stance[2] - 0.5 * W), stance[2] - side_max);
end walkTarget;

function actuatorLimit "Joint torque an electric actuator can deliver: smooth peak-torque saturation, derated linearly with speed"
  input Real tau_cmd "Commanded torque [N.m]";
  input Real w "Joint speed [rad/s]";
  input Real tau_peak "Peak torque at rest [N.m]";
  input Real w_max "No-load speed [rad/s]";
  output Real tau "Delivered torque [N.m]";
protected
  Real tau_avail;
algorithm
  // Available torque in the direction of motion falls to zero at the no-load speed; braking
  // (torque against the motion) keeps the full peak.
  if tau_cmd * w > 0 then
    tau_avail := tau_peak * max(0.05, 1 - abs(w) / w_max);
  else
    tau_avail := tau_peak;
  end if;
  tau := tau_avail * tanh(tau_cmd / tau_avail);
end actuatorLimit;
