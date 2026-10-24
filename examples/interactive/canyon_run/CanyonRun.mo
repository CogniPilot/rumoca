// CanyonRun: a fixed-wing aircraft learns to fly down a winding canyon.
//
// Everything that learns is ordinary Modelica in this file: the network
// layers, the loss gradient, the Adam optimizer and the replay ring. The
// aircraft is the 6-DOF plant and fly-by-wire controller of the fixed-wing
// example (../fixedwing/FixedWingSIL.mo), copied unchanged into Aircraft.
//
//   World frame : Z-up, the canyon runs along +x
//   Body frame  : FLU (x forward, y left, z up)
//   Quaternion  : {w, x, y, z} scalar-first, body-to-world
//
//   Aircraft: fixed-wing plant and fly-by-wire, from the fixed-wing example
//   Course  : canyon geometry and the local occupancy grid
//   Net     : the learner (dense layers, forward, backward, Adam)
//   Canyon  : builds the observation the agent sees every sample
//   Agent   : deep Q-learning in one sampled algorithm
//   Episode : reward, crash / finish / timeout and the episode counters
//   Run     : aircraft + canyon + agent + episode, wired together

package CanyonRun "Fixed-wing canyon flight learned with Q-learning written in Modelica"

  package Physics "The fixed-wing physics in a form both AD modes accept, for gradient checks"
    // The hybrid plant above switches on ground contact and on the wind-axis
    // reference, so trajectory gradients stop at those events. In flight the
    // gear is off the ground and |alpha| stays below 90 deg, so neither switch
    // can fire. AirborneFixedWing drops them and writes cross products, vector
    // literals and the quaternion functions out per component, which forward
    // (--grad-mode forward) and reverse (--grad-mode adjoint) AD both handle.

    partial model RigidBodyExplicit "CMM RigidBody6DOF with every product written per component"
      parameter Real mass = 1.0 "Mass [kg]";
      parameter Real g = 9.8 "Gravity [m/s^2]";
      parameter Real ixx = 1.0 "Body inertia matrix xx entry [kg*m^2]";
      parameter Real iyy = 1.0 "Body inertia matrix yy entry [kg*m^2]";
      parameter Real izz = 1.0 "Body inertia matrix zz entry [kg*m^2]";
      parameter Real ixy = 0.0 "Body inertia matrix xy entry [kg*m^2]";
      parameter Real ixz = 0.0 "Body inertia matrix xz entry [kg*m^2]";
      parameter Real iyz = 0.0 "Body inertia matrix yz entry [kg*m^2]";
      parameter Real J[3, 3] = [
        ixx, ixy, ixz;
        ixy, iyy, iyz;
        ixz, iyz, izz
      ] "Body inertia matrix [kg*m^2]";
      parameter Real p_start[3] = {0, 0, 0} "Initial world position";
      parameter Real v_b_start[3] = {0, 0, 0} "Initial body velocity";
      parameter Real q_start[4] = {1, 0, 0, 0} "Initial quaternion w,x,y,z";
      parameter Real omega_start[3] = {0, 0, 0} "Initial body angular velocity";
      parameter Real qnorm_gain = 1.0 "Quaternion renormalization gain";

      Real F_b[3] "Total non-gravity force in body frame [N]";
      Real M_b[3] "Total moment in body frame [N*m]";
      output Real p[3](start = p_start, each fixed = true) "World position [m]";
      output Real v_b[3](start = v_b_start, each fixed = true) "Body velocity [m/s]";
      output Real q[4](start = q_start, each fixed = true) "Quaternion w,x,y,z";
      output Real omega[3](start = omega_start, each fixed = true) "Body angular velocity [rad/s]";
      output Real R[3, 3](start = [
        1, 0, 0;
        0, 1, 0;
        0, 0, 1
      ]) "Direction cosine matrix, body to world";
      output Real v_w[3](start = v_b_start) "World velocity [m/s]";
      output Real a_b[3](start = {0, 0, 0}) "Body specific force [m/s^2]";

    protected
      Real q_dot_raw[4] "Unnormalized quaternion derivative";
      Real q_norm_err(start = 0) "Quaternion norm error";
      Real gravity_w[3] "Gravity in world frame [m/s^2]";
      Real gravity_b[3] "Gravity in body frame [m/s^2]";
      Real H_b[3] "Angular momentum in body frame [kg*m^2/s]";
      Real M_gyro[3] "Gyroscopic inertia moment in body frame [N*m]";
      Real M_body[3] "Rigid-body angular acceleration moment [N*m]";

    equation
      q_norm_err = q[1] * q[1] + q[2] * q[2] + q[3] * q[3] + q[4] * q[4] - 1;
      q_dot_raw[1] = 0.5*(-q[2]*omega[1] - q[3]*omega[2] - q[4]*omega[3]);
      q_dot_raw[2] = 0.5*( q[1]*omega[1] - q[4]*omega[2] + q[3]*omega[3]);
      q_dot_raw[3] = 0.5*( q[4]*omega[1] + q[1]*omega[2] - q[2]*omega[3]);
      q_dot_raw[4] = 0.5*(-q[3]*omega[1] + q[2]*omega[2] + q[1]*omega[3]);
      for i in 1:4 loop
        der(q[i]) = q_dot_raw[i] - qnorm_gain * q_norm_err * q[i];
      end for;
      R[1, 1] = q[1]*q[1] + q[2]*q[2] - q[3]*q[3] - q[4]*q[4];
      R[1, 2] = 2*(q[2]*q[3] - q[1]*q[4]);
      R[1, 3] = 2*(q[2]*q[4] + q[1]*q[3]);
      R[2, 1] = 2*(q[2]*q[3] + q[1]*q[4]);
      R[2, 2] = q[1]*q[1] - q[2]*q[2] + q[3]*q[3] - q[4]*q[4];
      R[2, 3] = 2*(q[3]*q[4] - q[1]*q[2]);
      R[3, 1] = 2*(q[2]*q[4] - q[1]*q[3]);
      R[3, 2] = 2*(q[3]*q[4] + q[1]*q[2]);
      R[3, 3] = q[1]*q[1] - q[2]*q[2] - q[3]*q[3] + q[4]*q[4];

      v_w = R * v_b;
      gravity_w[1] = 0;
      gravity_w[2] = 0;
      gravity_w[3] = -g;
      gravity_b = transpose(R) * gravity_w;
      a_b = F_b / mass;

      der(p) = v_w;
      der(v_b[1]) = a_b[1] + gravity_b[1] - (omega[2]*v_b[3] - omega[3]*v_b[2]);
      der(v_b[2]) = a_b[2] + gravity_b[2] - (omega[3]*v_b[1] - omega[1]*v_b[3]);
      der(v_b[3]) = a_b[3] + gravity_b[3] - (omega[1]*v_b[2] - omega[2]*v_b[1]);

      H_b = J * omega;
      M_gyro[1] = omega[2]*H_b[3] - omega[3]*H_b[2];
      M_gyro[2] = omega[3]*H_b[1] - omega[1]*H_b[3];
      M_gyro[3] = omega[1]*H_b[2] - omega[2]*H_b[1];
      M_body = M_b - M_gyro;
      J * der(omega) = M_body;
    end RigidBodyExplicit;

    model AirborneFixedWing "FixedWingPlant in flight: same aerodynamics, no landing gear, every product written per component"
      // Mass / inertia (small ~1.5 kg UAV, 1.5 m span)
      parameter Real vehicle_mass = 1.5 "Vehicle mass [kg]";
      parameter Real vehicle_ixx = 0.06 "Body inertia xx (roll) [kg*m^2]";
      parameter Real vehicle_iyy = 0.08 "Body inertia yy (pitch) [kg*m^2]";
      parameter Real vehicle_izz = 0.13 "Body inertia zz (yaw) [kg*m^2]";

      // Start at rest on the ground (wheels just touching), ready to take off.
      extends RigidBodyExplicit(
        mass = vehicle_mass,
        g = 9.81,
        ixx = vehicle_ixx,
        iyy = vehicle_iyy,
        izz = vehicle_izz,
        p_start = {0, 0, 0.149},
        v_b_start = {0, 0, 0},
        qnorm_gain = 1.0
      );

      // Reference geometry
      parameter Real rho = 1.225 "Air density [kg/m^3]";
      parameter Real S = 0.25 "Wing reference area [m^2]";
      parameter Real span = 1.5 "Wing span [m]";
      parameter Real cbar = 0.167 "Mean aerodynamic chord [m]";
      parameter Real wing_incidence = 0.10472 "Wing incidence angle [rad] (6 deg, cubs2)";
      parameter Real thr_max = 12.0 "Maximum thrust [N]";

      // Tricycle landing gear — contact points in body FLU, matched to the GLB
      // wheels at the visual scale (1 unit = 1 m). Per-wheel spring-damper normal
      // force, light rolling resistance (forward) and firmer lateral grip so it
      // tracks straight on the ground and absorbs landings.
      parameter Real ground_k = 3000 "Gear stiffness per wheel [N/m]";
      parameter Real ground_c = 150 "Gear normal damping per wheel [N*s/m]";
      parameter Real roll_fric = 0.1 "Rolling resistance [N/(m/s)]";
      parameter Real side_fric = 25 "Lateral grip [N/(m/s)]";
      parameter Real wheel_x[3] = {0.164, -0.173, -0.173} "Wheel fwd offsets nose,L,R [m]";
      parameter Real wheel_y[3] = {0.0, 0.185, -0.185} "Wheel left offsets [m]";
      parameter Real wheel_z[3] = {-0.151, -0.151, -0.151} "Wheel down offsets [m]";

      // Longitudinal coefficients (identified SportCub, cubs2)
      parameter Real CL0 = 0.5 "Lift at zero AoA";
      parameter Real CLa = 4.7 "Lift slope [1/rad]";
      parameter Real CD0 = 0.06 "Parasitic drag";
      parameter Real k_ind = 0.09 "Induced drag factor";
      parameter Real CD0_fp = 0.30 "Flat-plate drag (post-stall)";
      parameter Real Cm0 = 0.0 "Pitch moment at alpha=0";
      parameter Real Cma = -0.8 "Pitch stiffness (static stability) [1/rad]";
      parameter Real Cmq = -12.0 "Pitch damping";
      parameter Real Cmde = 0.3 "Elevator pitch effectiveness";

      // Lateral / directional coefficients (identified SportCub, cubs2)
      parameter Real CYb = -0.50 "Sideslip side force [1/rad]";
      parameter Real CYda = 0.004 "Aileron side force";
      parameter Real CYdr = -0.015 "Rudder side force";
      parameter Real CYp = -0.15 "Roll-rate side force";
      parameter Real CYr = 0.20 "Yaw-rate side force";
      parameter Real CY_fp_coef = 0.50 "Flat-plate side force (post-stall)";
      parameter Real Clb = -0.25 "Dihedral effect (roll due to sideslip)";
      parameter Real Clp = -0.50 "Roll damping";
      parameter Real Clr = 0.15 "Yaw-roll coupling";
      parameter Real Clda = 0.05 "Aileron roll effectiveness";
      parameter Real Cldr = 0.006 "Rudder roll";
      parameter Real Cnb = 0.06 "Weathercock stability";
      parameter Real Cnp = 0.010 "Roll-yaw coupling";
      parameter Real Cnr = -0.15 "Yaw damping";
      parameter Real Cndr = 0.015 "Rudder yaw effectiveness";
      parameter Real Cnda = 0.006 "Aileron adverse yaw";

      // Stall blending (flat-plate beyond alpha_stall, cubs2)
      parameter Real alpha_stall = 0.349 "Stall angle [rad] (20 deg)";
      parameter Real blend_width = 0.0873 "Stall blend width [rad] (5 deg)";

      // Control-surface travel (cubs2)
      parameter Real max_defl_ail = 0.5236 "Aileron travel [rad] (30 deg)";
      parameter Real max_defl_elev = 0.4189 "Elevator travel [rad] (24 deg)";
      parameter Real max_defl_rud = 0.349 "Rudder travel [rad] (20 deg)";

      parameter Real eps = 1e-6;

      // Normalized pilot/controller commands
      input Real ail "Aileron command [-1..1]";
      input Real elev "Elevator command [-1..1]";
      input Real rud "Rudder command [-1..1]";
      input Real thr "Throttle command [0..1]";

      // Viewer / controller outputs
      output Real position[3](start = p_start) "World position [m]";
      output Real velocity[3](start = v_b_start) "World velocity [m/s]";
      output Real quat[4](start = q_start) "Quaternion w,x,y,z";
      output Real gyro[3](start = {0, 0, 0}) "Body angular rate FLU [rad/s]";
      output Real up_body[3](start = {0, 0, 1}) "World up expressed in body FLU";
      output Real airspeed(start = 15) "True airspeed [m/s]";
      output Real alpha_deg(start = 0) "Angle of attack [deg]";
      output Real ail_rad(start = 0) "Aileron deflection [rad]";
      output Real elev_rad(start = 0) "Elevator deflection [rad]";
      output Real rud_rad(start = 0) "Rudder deflection [rad]";
      output Real thr_out(start = 0) "Throttle fraction [0..1]";

    protected
      Real U, V_frd, W_frd "Body velocity in FRD axes [m/s]";
      Real Vt, Vxz, alpha_body, alpha, beta, qbar, sigma;
      Real P_frd, Q_frd, R_frd "Body rates in FRD axes [rad/s]";
      Real wx1, wx2, wx3, wy1, wy2, wy3, wz1, wz2, wz3 "Wind-frame axes in body FRD";
      Real refx, refz, rdot, wzt1, wzt2, wzt3, nz "Wind-frame Gram-Schmidt temporaries";
      Real CL_lin, CL_fp, CL, CD_lin, CD_fp, CD, CY_lin, CY_fp, CY;
      Real Cl_aero, Cm_aero, Cn_aero;
      Real FA_frd[3] "Aero force in body FRD [N]";
      Real MA_frd[3] "Aero moment in body FRD [N*m]";
      Real F_aero[3], F_thrust[3], M_aero[3] "In body FLU";
      Real F_ground[3], M_ground[3] "Total ground wrench in body FLU";

    equation
      // --- FLU body velocity / rates -> FRD aero convention ---
      U = v_b[1];
      V_frd = -v_b[2];
      W_frd = -v_b[3];
      Vt = sqrt(U*U + V_frd*V_frd + W_frd*W_frd) + eps;
      Vxz = sqrt(U*U + W_frd*W_frd) + eps;
      alpha_body = atan2(W_frd, U);
      alpha = alpha_body + wing_incidence;
      beta = atan2(V_frd, Vxz);
      qbar = 0.5*rho*Vt*Vt;
      sigma = (1 + tanh((alpha - alpha_stall)/blend_width))/2;
      P_frd = omega[1];
      Q_frd = -omega[2];
      R_frd = -omega[3];

      // --- wind-frame axes from velocity (branch-free Gram-Schmidt, cubs2) ---
      wx1 = U/Vt; wx2 = V_frd/Vt; wx3 = W_frd/Vt;
      // reference = body axis most perpendicular to velocity (avoids singularity)
      refx = 0 "In flight |alpha| < 90 deg, so the body z axis is always the reference";
      refz = 1;
      rdot = refx*wx1 + refz*wx3;
      wzt1 = refx - rdot*wx1;
      wzt2 = -rdot*wx2;
      wzt3 = refz - rdot*wx3;
      nz = sqrt(wzt1*wzt1 + wzt2*wzt2 + wzt3*wzt3) + eps;
      wz1 = wzt1/nz; wz2 = wzt2/nz; wz3 = wzt3/nz;
      wy1 = wz2*wx3 - wz3*wx2;
      wy2 = wz3*wx1 - wz1*wx3;
      wy3 = wz1*wx2 - wz2*wx1;

      // --- surface deflections ---
      ail_rad = max_defl_ail*min(1, max(-1, ail));
      elev_rad = max_defl_elev*min(1, max(-1, elev));
      rud_rad = -max_defl_rud*min(1, max(-1, rud));  // cubs2 polarity (positive cmd -> +yaw)
      thr_out = min(1, max(0, thr));

      // --- aerodynamic coefficients (stability axes, smooth stall blend) ---
      CL_lin = CL0 + CLa*alpha;
      CL_fp = 2*sin(alpha)*cos(alpha);
      CL = (1 - sigma)*CL_lin + sigma*CL_fp;
      CD_lin = CD0 + k_ind*CL_lin*CL_lin;
      CD_fp = CD0_fp + 2*sin(alpha)*sin(alpha);
      CD = (1 - sigma)*CD_lin + sigma*CD_fp;
      CY_lin = CYb*beta + CYda*ail_rad + CYdr*rud_rad + CYp*(span/(2*Vt))*P_frd + CYr*(span/(2*Vt))*R_frd;
      CY_fp = CY_fp_coef*sin(beta)*cos(alpha);
      CY = (1 - sigma)*CY_lin + sigma*CY_fp;
      Cl_aero = Clda*ail_rad + Cldr*rud_rad + Clb*beta + Clp*(span/(2*Vt))*P_frd + Clr*(span/(2*Vt))*R_frd;
      Cm_aero = Cm0 + Cma*alpha + Cmde*elev_rad + Cmq*(cbar/(2*Vt))*Q_frd;
      Cn_aero = Cnb*beta + Cndr*rud_rad + Cnda*ail_rad + Cnp*(span/(2*Vt))*P_frd + Cnr*(span/(2*Vt))*R_frd;

      // --- wind axes -> body FRD via R_b_wind (columns wx, wy, wz),
      //     FA_wind = qbar*S*{-CD, CY, -CL} ---
      FA_frd[1] = qbar*S*(wx1*(-CD) + wy1*CY + wz1*(-CL));
      FA_frd[2] = qbar*S*(wx2*(-CD) + wy2*CY + wz2*(-CL));
      FA_frd[3] = qbar*S*(wx3*(-CD) + wy3*CY + wz3*(-CL));
      MA_frd[1] = qbar*S*span*Cl_aero;
      MA_frd[2] = qbar*S*cbar*Cm_aero;
      MA_frd[3] = qbar*S*span*Cn_aero;

      // --- body FRD -> body FLU (negate y, z) ---
      F_aero[1] = FA_frd[1];
      F_aero[2] = -FA_frd[2];
      F_aero[3] = -FA_frd[3];
      M_aero[1] = MA_frd[1];
      M_aero[2] = -MA_frd[2];
      M_aero[3] = -MA_frd[3];
      F_thrust[1] = thr_max*thr_out;
      F_thrust[2] = 0;
      F_thrust[3] = 0;

      // --- in flight: no ground contact ---
      for k in 1:3 loop
        F_ground[k] = 0;
        M_ground[k] = 0;
      end for;

      // --- hand non-gravity wrench to the rigid-body integrator ---
      F_b = F_aero + F_thrust + F_ground;
      M_b = M_aero + M_ground;

      // --- viewer outputs ---
      gyro = omega;
      up_body = R[3, :];
      airspeed = Vt;
      alpha_deg = alpha*57.29577951308232;
      position = p;
      velocity = v_w;
      quat = q;
    end AirborneFixedWing;

    model Glide "An airborne flight with constant controls: what the gradients are taken of"
      parameter Real elevator = 0.05 "Elevator command [-1..1]";
      parameter Real aileron = 0.02 "Aileron command [-1..1]";
      parameter Real throttleSetting = 0.5 "Throttle [0..1]";
      AirborneFixedWing vehicle(p_start = {0, 0, 30}, v_b_start = {15, 0, 0});
      output Real x = vehicle.position[1] "Downrange [m]";
      output Real y = vehicle.position[2] "Crossrange [m]";
      output Real z = vehicle.position[3] "Altitude [m]";
    equation
      vehicle.ail = aileron;
      vehicle.elev = elevator;
      vehicle.rud = 0;
      vehicle.thr = throttleSetting;
      annotation(experiment(StopTime = 3, Interval = 0.01, Solver = "rk-like"));
    end Glide;

    model HybridGlide "The same flight on the original hybrid plant, for the equivalence check"
      parameter Real elevator = 0.05;
      parameter Real aileron = 0.02;
      parameter Real throttleSetting = 0.5;
      Aircraft.FixedWingPlant vehicle(p_start = {0, 0, 30}, v_b_start = {15, 0, 0});
      output Real x = vehicle.position[1];
      output Real y = vehicle.position[2];
      output Real z = vehicle.position[3];
    equation
      vehicle.ail = aileron;
      vehicle.elev = elevator;
      vehicle.rud = 0;
      vehicle.thr = throttleSetting;
      annotation(experiment(StopTime = 3, Interval = 0.01, Solver = "rk-like"));
    end HybridGlide;
  end Physics;

  package Course "Canyon geometry and the local occupancy grid"
    constant Real rimHeight = 30 "Canyon rim height above the floor [m]";
    constant Real length = 600 "Distance to the finish line [m]";
    constant Real startX = 0 "Start position along the canyon [m]";
    constant Real startHeight = 12 "Start height above the floor [m]";
    constant Real startSpeed = 15 "Start airspeed [m/s]";

    constant Real gridAhead = 40 "Occupancy grid reach ahead of the craft [m]";
    constant Real gridAcross = 40 "Occupancy grid width across the craft [m]";

    function centreLine "Canyon centreline y at x [m]"
      input Real x;
      output Real y;
    algorithm
      y := 10*sin(0.0349*x) + 1.5*sin(0.0898*x + 1.3);
    end centreLine;

    function centreSlope "dy/dx of the centreline at x"
      input Real x;
      output Real s;
    algorithm
      s := 10*0.0349*cos(0.0349*x) + 1.5*0.0898*cos(0.0898*x + 1.3);
    end centreSlope;

    function halfWidth "Canyon half-width at x [m]"
      input Real x;
      output Real w;
    algorithm
      w := 16 + 3*sin(0.0524*x + 0.7);
    end halfWidth;

    function inFreeAir "True when a point is in open air: above the floor and between the walls or above the rim"
      input Real x;
      input Real y;
      input Real z;
      output Boolean free;
    algorithm
      free := z > 0 and (z > rimHeight or abs(y - centreLine(x)) < halfWidth(x));
    end inFreeAir;

    function rotate "Rotate a body vector into the world frame"
      input Real q[4] "Quaternion w,x,y,z, body-to-world";
      input Real b[3];
      output Real w[3];
    algorithm
      w := {(1 - 2*(q[3]*q[3] + q[4]*q[4]))*b[1] + 2*(q[2]*q[3] - q[1]*q[4])*b[2] + 2*(q[2]*q[4] + q[1]*q[3])*b[3],
            2*(q[2]*q[3] + q[1]*q[4])*b[1] + (1 - 2*(q[2]*q[2] + q[4]*q[4]))*b[2] + 2*(q[3]*q[4] - q[1]*q[2])*b[3],
            2*(q[2]*q[4] - q[1]*q[3])*b[1] + 2*(q[3]*q[4] + q[1]*q[2])*b[2] + (1 - 2*(q[2]*q[2] + q[3]*q[3]))*b[3]};
    end rotate;

    function occupancy "Local occupancy ahead of the craft: the share of four sample points in each cell outside the canyon"
      input Real p[3] "Position [m]";
      input Real q[4] "Attitude; only the heading is used";
      input Integer nRows "Rows, 1 = farthest ahead";
      input Integer nCols "Columns, 1 = leftmost";
      output Real occ[nRows*nCols];
    protected
      Real fwd[2] "Heading direction in the horizontal plane";
      Real left[2];
      Real n;
      Real ahead;
      Real across;
      Real x;
      Real y;
      Integer outside;
    algorithm
      // The grid turns with the craft's heading but stays level, so a cell
      // always means the same place relative to where the craft is going.
      fwd := {1 - 2*(q[3]*q[3] + q[4]*q[4]), 2*(q[2]*q[3] + q[1]*q[4])};
      n := sqrt(fwd[1]*fwd[1] + fwd[2]*fwd[2]) + 1e-9;
      fwd := fwd/n;
      left := {-fwd[2], fwd[1]};
      for i in 1:nRows loop
        for j in 1:nCols loop
          outside := 0;
          for si in 1:2 loop
            for sj in 1:2 loop
              ahead := gridAhead*(nRows - i + 0.25 + 0.5*(si - 1))/nRows;
              across := gridAcross*(0.5 - (j - 0.75 + 0.5*(sj - 1))/nCols);
              x := p[1] + ahead*fwd[1] + across*left[1];
              y := p[2] + ahead*fwd[2] + across*left[2];
              if abs(y - centreLine(x)) >= halfWidth(x) then
                outside := outside + 1;
              end if;
            end for;
          end for;
          occ[(i - 1)*nCols + j] := outside/4;
        end for;
      end for;
    end occupancy;

    function features "The 15 true-state features, normalised"
      input Real p[3];
      input Real q[4];
      input Real v[3];
      input Real w[3];
      input Real sp[2] "Fly-by-wire held bank and pitch setpoints [rad]";
      output Real f[15];
    algorithm
      f := {(p[2] - centreLine(p[1]))/halfWidth(p[1]),
            (p[3] - 0.5*rimHeight)/(0.5*rimHeight),
            centreSlope(p[1]),
            q[1], q[2], q[3], q[4],
            v[1]/startSpeed, v[2]/startSpeed, v[3]/startSpeed,
            w[1]/2, w[2]/2, w[3]/2,
            sp[1], sp[2]};
    end features;

    function startAttitude "Level attitude pointing down the canyon at the start"
      output Real q[4];
    protected
      Real psi;
    algorithm
      psi := atan(centreSlope(startX));
      q := {cos(psi/2), 0, 0, sin(psi/2)};
    end startAttitude;
  end Course;

  package Net "The learner, all plain Modelica functions"
    function relu "Elementwise max(x, 0)"
      input Real x[:];
      output Real y[size(x, 1)];
    algorithm
      y := 0.5*(x + abs(x));
    end relu;

    function reluSlope "Derivative of relu: 1 where x > 0, else 0"
      input Real x[:];
      output Real s[size(x, 1)];
    algorithm
      for i in 1:size(x, 1) loop
        s[i] := if x[i] > 0 then 1 else 0;
      end for;
    end reluSlope;

    function dense "y = W*x + b"
      input Real W[:, :];
      input Real b[size(W, 1)];
      input Real x[size(W, 2)];
      output Real y[size(W, 1)];
    algorithm
      y := W*x + b;
    end dense;

    function outerProd "Outer product u*v'"
      input Real u[:];
      input Real v[:];
      output Real M[size(u, 1), size(v, 1)];
    algorithm
      for i in 1:size(u, 1) loop
        for j in 1:size(v, 1) loop
          M[i, j] := u[i]*v[j];
        end for;
      end for;
    end outerProd;

    function forward "Q-value of every action for observation x"
      input Real W1[:, :];
      input Real b1[size(W1, 1)];
      input Real W2[:, size(W1, 1)];
      input Real b2[size(W2, 1)];
      input Real W3[:, size(W2, 1)];
      input Real b3[size(W3, 1)];
      input Real x[size(W1, 2)];
      output Real q[size(W3, 1)];
    protected
      Real h1[size(W1, 1)];
      Real h2[size(W2, 1)];
    algorithm
      h1 := relu(dense(W1, b1, x));
      h2 := relu(dense(W2, b2, h1));
      q := dense(W3, b3, h2);
    end forward;

    function tdTarget "r + gamma * max_a' Q_target(s', a'), or just r when the episode ended"
      input Real r "Reward";
      input Boolean done "The transition ended the episode";
      input Real qNext[:] "Target-network Q-values at the next observation";
      input Real gamma "Discount per step";
      output Real target;
    algorithm
      target := if done then r else r + gamma*qNext[argmax(qNext)];
    end tdTarget;

    function backward "Gradient of the Huber TD loss of action a, one layer at a time"
      input Real W1[:, :];
      input Real b1[size(W1, 1)];
      input Real W2[:, size(W1, 1)];
      input Real b2[size(W2, 1)];
      input Real W3[:, size(W2, 1)];
      input Real b3[size(W3, 1)];
      input Real x[size(W1, 2)];
      input Integer a "Action taken";
      input Real target "TD target for Q(x, a)";
      output Real gW1[size(W1, 1), size(W1, 2)];
      output Real gb1[size(W1, 1)];
      output Real gW2[size(W2, 1), size(W2, 2)];
      output Real gb2[size(W2, 1)];
      output Real gW3[size(W3, 1), size(W3, 2)];
      output Real gb3[size(W3, 1)];
    protected
      Real z1[size(W1, 1)];
      Real h1[size(W1, 1)];
      Real z2[size(W2, 1)];
      Real h2[size(W2, 1)];
      Real q[size(W3, 1)];
      Real d2[size(W2, 1)];
      Real d1[size(W1, 1)];
    algorithm
      z1 := dense(W1, b1, x);
      h1 := relu(z1);
      z2 := dense(W2, b2, h1);
      h2 := relu(z2);
      q := dense(W3, b3, h2);
      gb3 := zeros(size(W3, 1));
      gb3[a] := max(-1, min(1, q[a] - target));
      gW3 := outerProd(gb3, h2);
      d2 := (transpose(W3)*gb3) .* reluSlope(z2);
      gb2 := d2;
      gW2 := outerProd(d2, h1);
      d1 := (transpose(W2)*d2) .* reluSlope(z1);
      gb1 := d1;
      gW1 := outerProd(d1, x);
    end backward;

    function adamStep "One Adam update of a weight matrix"
      input Real w[:, :];
      input Real g[size(w, 1), size(w, 2)];
      input Real m[size(w, 1), size(w, 2)];
      input Real v[size(w, 1), size(w, 2)];
      input Integer t "Update count, from 1";
      input Real rate "Learning rate";
      output Real wNew[size(w, 1), size(w, 2)];
      output Real mNew[size(w, 1), size(w, 2)];
      output Real vNew[size(w, 1), size(w, 2)];
    protected
      constant Real beta1 = 0.9;
      constant Real beta2 = 0.999;
      constant Real eps = 1e-8;
    algorithm
      // Per weight: m = b1*m + (1-b1)*g, v = b2*v + (1-b2)*g^2,
      // w = w - rate * (m/(1-b1^t)) / (sqrt(v/(1-b2^t)) + eps).
      mNew := beta1*m + (1 - beta1)*g;
      vNew := beta2*v + (1 - beta2)*(g .* g);
      wNew := w - rate*(mNew/(1 - beta1^t)) ./ (sqrt(vNew/(1 - beta2^t)) .+ eps);
    end adamStep;

    function adamStepVector "One Adam update of a bias vector"
      input Real w[:];
      input Real g[size(w, 1)];
      input Real m[size(w, 1)];
      input Real v[size(w, 1)];
      input Integer t "Update count, from 1";
      input Real rate "Learning rate";
      output Real wNew[size(w, 1)];
      output Real mNew[size(w, 1)];
      output Real vNew[size(w, 1)];
    protected
      constant Real beta1 = 0.9;
      constant Real beta2 = 0.999;
      constant Real eps = 1e-8;
    algorithm
      mNew := beta1*m + (1 - beta1)*g;
      vNew := beta2*v + (1 - beta2)*(g .* g);
      wNew := w - rate*(mNew/(1 - beta1^t)) ./ (sqrt(vNew/(1 - beta2^t)) .+ eps);
    end adamStepVector;

    function remember "Write one transition into the replay ring (oldest overwritten first)"
      input Real obsRing[:, :] "Observations";
      input Integer actRing[size(obsRing, 1)] "Actions taken";
      input Real rewardRing[size(obsRing, 1)] "Rewards earned";
      input Real nextRing[size(obsRing, 1), size(obsRing, 2)] "Observations that followed";
      input Boolean doneRing[size(obsRing, 1)] "Whether the episode ended there";
      input Integer stored "Transitions written so far";
      input Boolean valid "False on the first sample of an episode: nothing to store";
      input Real s[size(obsRing, 2)];
      input Integer a;
      input Real r;
      input Real sNext[size(obsRing, 2)];
      input Boolean done;
      output Real obsOut[size(obsRing, 1), size(obsRing, 2)];
      output Integer actOut[size(obsRing, 1)];
      output Real rewardOut[size(obsRing, 1)];
      output Real nextOut[size(obsRing, 1), size(obsRing, 2)];
      output Boolean doneOut[size(obsRing, 1)];
      output Integer storedOut;
    protected
      Integer slot;
    algorithm
      obsOut := obsRing;
      actOut := actRing;
      rewardOut := rewardRing;
      nextOut := nextRing;
      doneOut := doneRing;
      storedOut := stored;
      if valid then
        slot := mod(stored, size(obsRing, 1)) + 1;
        obsOut[slot, :] := s;
        actOut[slot] := a;
        rewardOut[slot] := r;
        nextOut[slot, :] := sNext;
        doneOut[slot] := done;
        storedOut := stored + 1;
      end if;
    end remember;

    function batchGradient "Mean TD-loss gradient over a batch drawn from the ring; zero until the ring holds a batch"
      input Real W1[:, :];
      input Real b1[size(W1, 1)];
      input Real W2[:, size(W1, 1)];
      input Real b2[size(W2, 1)];
      input Real W3[:, size(W2, 1)];
      input Real b3[size(W3, 1)];
      input Real T1[size(W1, 1), size(W1, 2)] "Target network";
      input Real c1[size(W1, 1)];
      input Real T2[size(W2, 1), size(W2, 2)];
      input Real c2[size(W2, 1)];
      input Real T3[size(W3, 1), size(W3, 2)];
      input Real c3[size(W3, 1)];
      input Real obsRing[:, size(W1, 2)];
      input Integer actRing[size(obsRing, 1)];
      input Real rewardRing[size(obsRing, 1)];
      input Real nextRing[size(obsRing, 1), size(W1, 2)];
      input Boolean doneRing[size(obsRing, 1)];
      input Integer filled "Ring entries holding a transition";
      input Integer batchSize;
      input Integer step "Draw counter";
      input Integer channel "Random stream";
      input Real gamma "Discount per step";
      output Real gW1[size(W1, 1), size(W1, 2)];
      output Real gb1[size(W1, 1)];
      output Real gW2[size(W2, 1), size(W2, 2)];
      output Real gb2[size(W2, 1)];
      output Real gW3[size(W3, 1), size(W3, 2)];
      output Real gb3[size(W3, 1)];
    protected
      Real dW1[size(W1, 1), size(W1, 2)];
      Real db1[size(W1, 1)];
      Real dW2[size(W2, 1), size(W2, 2)];
      Real db2[size(W2, 1)];
      Real dW3[size(W3, 1), size(W3, 2)];
      Real db3[size(W3, 1)];
      Integer pick;
      Real target;
    algorithm
      gW1 := zeros(size(W1, 1), size(W1, 2));
      gb1 := zeros(size(W1, 1));
      gW2 := zeros(size(W2, 1), size(W2, 2));
      gb2 := zeros(size(W2, 1));
      gW3 := zeros(size(W3, 1), size(W3, 2));
      gb3 := zeros(size(W3, 1));
      if filled >= batchSize then
        for k in 1:batchSize loop
          pick := min(filled, 1 + integer(hash01(step, k, channel)*filled));
          target := tdTarget(rewardRing[pick], doneRing[pick],
            forward(T1, c1, T2, c2, T3, c3, nextRing[pick, :]), gamma);
          (dW1, db1, dW2, db2, dW3, db3) := backward(W1, b1, W2, b2, W3, b3,
            obsRing[pick, :], actRing[pick], target);
          gW1 := gW1 + dW1/batchSize;
          gb1 := gb1 + db1/batchSize;
          gW2 := gW2 + dW2/batchSize;
          gb2 := gb2 + db2/batchSize;
          gW3 := gW3 + dW3/batchSize;
          gb3 := gb3 + db3/batchSize;
        end for;
      end if;
    end batchGradient;

    function chooseAction "Epsilon-greedy choice; the first sample of an episode uses the default action"
      input Real q[:] "Q-values of every action";
      input Real epsilon;
      input Integer step;
      input Integer channel;
      input Boolean episodeStart;
      input Integer defaultAction;
      output Integer action;
    algorithm
      if episodeStart then
        action := defaultAction;
      elseif hash01(step, 0, channel) < epsilon then
        action := min(size(q, 1), 1 + integer(hash01(step, size(q, 1), channel)*size(q, 1)));
      else
        action := argmax(q);
      end if;
    end chooseAction;

    function copyIf "a when copy is true, otherwise b"
      input Boolean copy;
      input Real a[:, :];
      input Real b[size(a, 1), size(a, 2)];
      output Real y[size(a, 1), size(a, 2)];
    algorithm
      y := if copy then a else b;
    end copyIf;

    function copyVectorIf "a when copy is true, otherwise b"
      input Boolean copy;
      input Real a[:];
      input Real b[size(a, 1)];
      output Real y[size(a, 1)];
    algorithm
      y := if copy then a else b;
    end copyVectorIf;

    function argmax "Index of the largest entry (first on ties)"
      input Real x[:];
      output Integer k;
    algorithm
      k := 1;
      for i in 2:size(x, 1) loop
        if x[i] > x[k] then
          k := i;
        end if;
      end for;
    end argmax;

    function hash01 "Uniform number in [0, 1) from integer counters, with no integer overflow"
      input Integer a;
      input Integer b;
      input Integer channel;
      output Real u;
    protected
      Integer x;
      Integer y;
      Integer z;
    algorithm
      // Three quadratic maps modulo primes below 30330, each fed by another, mix
      // the counters nonlinearly. Every intermediate stays below 2^31, so no
      // Integer arithmetic here can overflow.
      x := mod(a, 30269);
      y := mod(b + 7*channel, 30307);
      z := mod(mod(a, 30323) + 3*mod(b, 30323) + 11*channel + 5, 30323);
      for r in 1:4 loop
        x := mod(x*x + 171*y + 1, 30269);
        y := mod(y*y + 172*z + 3, 30307);
        z := mod(z*z + 170*x + 5, 30323);
      end for;
      u := x/30269.0 + y/30307.0 + z/30323.0;
      u := u - integer(u);
    end hash01;

    function initWeights "Deterministic start weights: uniform, with the variance suited to relu layers"
      input Integer rows;
      input Integer cols;
      input Integer layer;
      output Real W[rows, cols];
    algorithm
      for i in 1:rows loop
        for j in 1:cols loop
          W[i, j] := sqrt(6.0/cols)*(2*hash01(i, j, 100 + layer) - 1);
        end for;
      end for;
    end initWeights;
  end Net;

  package Aircraft "The fixed-wing plant and fly-by-wire controller"
    // FixedWingPlant and FixedWingFBW are copied unchanged from
    // ../fixedwing/FixedWingSIL.mo, the fixed-wing example this one builds on.
    // Keep the two in step when either changes.

    model FixedWingPlant "6-DOF fixed-wing aero plant on RigidBody6DOF"
      // Mass / inertia (small ~1.5 kg UAV, 1.5 m span)
      parameter Real vehicle_mass = 1.5 "Vehicle mass [kg]";
      parameter Real vehicle_ixx = 0.06 "Body inertia xx (roll) [kg*m^2]";
      parameter Real vehicle_iyy = 0.08 "Body inertia yy (pitch) [kg*m^2]";
      parameter Real vehicle_izz = 0.13 "Body inertia zz (yaw) [kg*m^2]";

      // Start at rest on the ground (wheels just touching), ready to take off.
      extends RigidBody.RigidBody6DOF(
        mass = vehicle_mass,
        g = 9.81,
        ixx = vehicle_ixx,
        iyy = vehicle_iyy,
        izz = vehicle_izz,
        p_start = {0, 0, 0.149},
        v_b_start = {0, 0, 0},
        qnorm_gain = 1.0
      );

      // Reference geometry
      parameter Real rho = 1.225 "Air density [kg/m^3]";
      parameter Real S = 0.25 "Wing reference area [m^2]";
      parameter Real span = 1.5 "Wing span [m]";
      parameter Real cbar = 0.167 "Mean aerodynamic chord [m]";
      parameter Real wing_incidence = 0.10472 "Wing incidence angle [rad] (6 deg, cubs2)";
      parameter Real thr_max = 12.0 "Maximum thrust [N]";

      // Tricycle landing gear — contact points in body FLU, matched to the GLB
      // wheels at the visual scale (1 unit = 1 m). Per-wheel spring-damper normal
      // force, light rolling resistance (forward) and firmer lateral grip so it
      // tracks straight on the ground and absorbs landings.
      parameter Real ground_k = 3000 "Gear stiffness per wheel [N/m]";
      parameter Real ground_c = 150 "Gear normal damping per wheel [N*s/m]";
      parameter Real roll_fric = 0.1 "Rolling resistance [N/(m/s)]";
      parameter Real side_fric = 25 "Lateral grip [N/(m/s)]";
      parameter Real wheel_x[3] = {0.164, -0.173, -0.173} "Wheel fwd offsets nose,L,R [m]";
      parameter Real wheel_y[3] = {0.0, 0.185, -0.185} "Wheel left offsets [m]";
      parameter Real wheel_z[3] = {-0.151, -0.151, -0.151} "Wheel down offsets [m]";

      // Longitudinal coefficients (identified SportCub, cubs2)
      parameter Real CL0 = 0.5 "Lift at zero AoA";
      parameter Real CLa = 4.7 "Lift slope [1/rad]";
      parameter Real CD0 = 0.06 "Parasitic drag";
      parameter Real k_ind = 0.09 "Induced drag factor";
      parameter Real CD0_fp = 0.30 "Flat-plate drag (post-stall)";
      parameter Real Cm0 = 0.0 "Pitch moment at alpha=0";
      parameter Real Cma = -0.8 "Pitch stiffness (static stability) [1/rad]";
      parameter Real Cmq = -12.0 "Pitch damping";
      parameter Real Cmde = 0.3 "Elevator pitch effectiveness";

      // Lateral / directional coefficients (identified SportCub, cubs2)
      parameter Real CYb = -0.50 "Sideslip side force [1/rad]";
      parameter Real CYda = 0.004 "Aileron side force";
      parameter Real CYdr = -0.015 "Rudder side force";
      parameter Real CYp = -0.15 "Roll-rate side force";
      parameter Real CYr = 0.20 "Yaw-rate side force";
      parameter Real CY_fp_coef = 0.50 "Flat-plate side force (post-stall)";
      parameter Real Clb = -0.25 "Dihedral effect (roll due to sideslip)";
      parameter Real Clp = -0.50 "Roll damping";
      parameter Real Clr = 0.15 "Yaw-roll coupling";
      parameter Real Clda = 0.05 "Aileron roll effectiveness";
      parameter Real Cldr = 0.006 "Rudder roll";
      parameter Real Cnb = 0.06 "Weathercock stability";
      parameter Real Cnp = 0.010 "Roll-yaw coupling";
      parameter Real Cnr = -0.15 "Yaw damping";
      parameter Real Cndr = 0.015 "Rudder yaw effectiveness";
      parameter Real Cnda = 0.006 "Aileron adverse yaw";

      // Stall blending (flat-plate beyond alpha_stall, cubs2)
      parameter Real alpha_stall = 0.349 "Stall angle [rad] (20 deg)";
      parameter Real blend_width = 0.0873 "Stall blend width [rad] (5 deg)";

      // Control-surface travel (cubs2)
      parameter Real max_defl_ail = 0.5236 "Aileron travel [rad] (30 deg)";
      parameter Real max_defl_elev = 0.4189 "Elevator travel [rad] (24 deg)";
      parameter Real max_defl_rud = 0.349 "Rudder travel [rad] (20 deg)";

      parameter Real eps = 1e-6;

      // Normalized pilot/controller commands
      input Real ail "Aileron command [-1..1]";
      input Real elev "Elevator command [-1..1]";
      input Real rud "Rudder command [-1..1]";
      input Real thr "Throttle command [0..1]";

      // Viewer / controller outputs
      output Real position[3](start = p_start) "World position [m]";
      output Real velocity[3](start = v_b_start) "World velocity [m/s]";
      output Real quat[4](start = q_start) "Quaternion w,x,y,z";
      output Real gyro[3](start = {0, 0, 0}) "Body angular rate FLU [rad/s]";
      output Real up_body[3](start = {0, 0, 1}) "World up expressed in body FLU";
      output Real airspeed(start = 15) "True airspeed [m/s]";
      output Real alpha_deg(start = 0) "Angle of attack [deg]";
      output Real ail_rad(start = 0) "Aileron deflection [rad]";
      output Real elev_rad(start = 0) "Elevator deflection [rad]";
      output Real rud_rad(start = 0) "Rudder deflection [rad]";
      output Real thr_out(start = 0) "Throttle fraction [0..1]";

    protected
      Real U, V_frd, W_frd "Body velocity in FRD axes [m/s]";
      Real Vt, Vxz, alpha_body, alpha, beta, qbar, sigma;
      Real P_frd, Q_frd, R_frd "Body rates in FRD axes [rad/s]";
      Real wx1, wx2, wx3, wy1, wy2, wy3, wz1, wz2, wz3 "Wind-frame axes in body FRD";
      Real refx, refz, rdot, wzt1, wzt2, wzt3, nz "Wind-frame Gram-Schmidt temporaries";
      Real CL_lin, CL_fp, CL, CD_lin, CD_fp, CD, CY_lin, CY_fp, CY;
      Real Cl_aero, Cm_aero, Cn_aero;
      Real FA_frd[3] "Aero force in body FRD [N]";
      Real MA_frd[3] "Aero moment in body FRD [N*m]";
      Real F_aero[3], F_thrust[3], M_aero[3] "In body FLU";
      Real wh_h[3], wh_vbx[3], wh_vby[3], wh_vbz[3], wh_vwz[3], wh_Fn[3];
      Real wh_F[3, 3], wh_M[3, 3] "Per-wheel force / moment in body FLU";
      Real F_ground[3], M_ground[3] "Total ground wrench in body FLU";

    equation
      // --- FLU body velocity / rates -> FRD aero convention ---
      U = v_b[1];
      V_frd = -v_b[2];
      W_frd = -v_b[3];
      Vt = sqrt(U*U + V_frd*V_frd + W_frd*W_frd) + eps;
      Vxz = sqrt(U*U + W_frd*W_frd) + eps;
      alpha_body = atan2(W_frd, U);
      alpha = alpha_body + wing_incidence;
      beta = atan2(V_frd, Vxz);
      qbar = 0.5*rho*Vt*Vt;
      sigma = (1 + tanh((alpha - alpha_stall)/blend_width))/2;
      P_frd = omega[1];
      Q_frd = -omega[2];
      R_frd = -omega[3];

      // --- wind-frame axes from velocity (branch-free Gram-Schmidt, cubs2) ---
      wx1 = U/Vt; wx2 = V_frd/Vt; wx3 = W_frd/Vt;
      // reference = body axis most perpendicular to velocity (avoids singularity)
      refx = if abs(wx3) < abs(wx1) then 0 else 1;
      refz = if abs(wx3) < abs(wx1) then 1 else 0;
      rdot = refx*wx1 + refz*wx3;
      wzt1 = refx - rdot*wx1;
      wzt2 = -rdot*wx2;
      wzt3 = refz - rdot*wx3;
      nz = sqrt(wzt1*wzt1 + wzt2*wzt2 + wzt3*wzt3) + eps;
      wz1 = wzt1/nz; wz2 = wzt2/nz; wz3 = wzt3/nz;
      wy1 = wz2*wx3 - wz3*wx2;
      wy2 = wz3*wx1 - wz1*wx3;
      wy3 = wz1*wx2 - wz2*wx1;

      // --- surface deflections ---
      ail_rad = max_defl_ail*min(1, max(-1, ail));
      elev_rad = max_defl_elev*min(1, max(-1, elev));
      rud_rad = -max_defl_rud*min(1, max(-1, rud));  // cubs2 polarity (positive cmd -> +yaw)
      thr_out = min(1, max(0, thr));

      // --- aerodynamic coefficients (stability axes, smooth stall blend) ---
      CL_lin = CL0 + CLa*alpha;
      CL_fp = 2*sin(alpha)*cos(alpha);
      CL = (1 - sigma)*CL_lin + sigma*CL_fp;
      CD_lin = CD0 + k_ind*CL_lin*CL_lin;
      CD_fp = CD0_fp + 2*sin(alpha)*sin(alpha);
      CD = (1 - sigma)*CD_lin + sigma*CD_fp;
      CY_lin = CYb*beta + CYda*ail_rad + CYdr*rud_rad + CYp*(span/(2*Vt))*P_frd + CYr*(span/(2*Vt))*R_frd;
      CY_fp = CY_fp_coef*sin(beta)*cos(alpha);
      CY = (1 - sigma)*CY_lin + sigma*CY_fp;
      Cl_aero = Clda*ail_rad + Cldr*rud_rad + Clb*beta + Clp*(span/(2*Vt))*P_frd + Clr*(span/(2*Vt))*R_frd;
      Cm_aero = Cm0 + Cma*alpha + Cmde*elev_rad + Cmq*(cbar/(2*Vt))*Q_frd;
      Cn_aero = Cnb*beta + Cndr*rud_rad + Cnda*ail_rad + Cnp*(span/(2*Vt))*P_frd + Cnr*(span/(2*Vt))*R_frd;

      // --- wind axes -> body FRD via R_b_wind (columns wx, wy, wz),
      //     FA_wind = qbar*S*{-CD, CY, -CL} ---
      FA_frd[1] = qbar*S*(wx1*(-CD) + wy1*CY + wz1*(-CL));
      FA_frd[2] = qbar*S*(wx2*(-CD) + wy2*CY + wz2*(-CL));
      FA_frd[3] = qbar*S*(wx3*(-CD) + wy3*CY + wz3*(-CL));
      MA_frd[1] = qbar*S*span*Cl_aero;
      MA_frd[2] = qbar*S*cbar*Cm_aero;
      MA_frd[3] = qbar*S*span*Cn_aero;

      // --- body FRD -> body FLU (negate y, z) ---
      F_aero = {FA_frd[1], -FA_frd[2], -FA_frd[3]};
      M_aero = {MA_frd[1], -MA_frd[2], -MA_frd[3]};
      F_thrust = {thr_max*thr_out, 0, 0};

      // --- tricycle landing-gear contact (spring-damper per wheel) ---
      // Contact height uses world-up (R[3,:]); normal force acts along world-up
      // expressed in body (= R[3,:]); friction is applied in the body tangential
      // plane (low forward, firm lateral). Moments are r x F about the CG.
      for i in 1:3 loop
        wh_h[i] = p[3] + R[3, 1]*wheel_x[i] + R[3, 2]*wheel_y[i] + R[3, 3]*wheel_z[i];
        wh_vbx[i] = v_b[1] + omega[2]*wheel_z[i] - omega[3]*wheel_y[i];
        wh_vby[i] = v_b[2] + omega[3]*wheel_x[i] - omega[1]*wheel_z[i];
        wh_vbz[i] = v_b[3] + omega[1]*wheel_y[i] - omega[2]*wheel_x[i];
        wh_vwz[i] = R[3, 1]*wh_vbx[i] + R[3, 2]*wh_vby[i] + R[3, 3]*wh_vbz[i];
        wh_Fn[i] = if wh_h[i] < 0 then max(0, ground_k*(-wh_h[i]) - ground_c*wh_vwz[i]) else 0;
        wh_F[1, i] = wh_Fn[i]*R[3, 1] + (if wh_h[i] < 0 then -roll_fric*wh_vbx[i] else 0);
        wh_F[2, i] = wh_Fn[i]*R[3, 2] + (if wh_h[i] < 0 then -side_fric*wh_vby[i] else 0);
        wh_F[3, i] = wh_Fn[i]*R[3, 3];
        wh_M[1, i] = wheel_y[i]*wh_F[3, i] - wheel_z[i]*wh_F[2, i];
        wh_M[2, i] = wheel_z[i]*wh_F[1, i] - wheel_x[i]*wh_F[3, i];
        wh_M[3, i] = wheel_x[i]*wh_F[2, i] - wheel_y[i]*wh_F[1, i];
      end for;
      F_ground = {wh_F[1, 1] + wh_F[1, 2] + wh_F[1, 3],
                  wh_F[2, 1] + wh_F[2, 2] + wh_F[2, 3],
                  wh_F[3, 1] + wh_F[3, 2] + wh_F[3, 3]};
      M_ground = {wh_M[1, 1] + wh_M[1, 2] + wh_M[1, 3],
                  wh_M[2, 1] + wh_M[2, 2] + wh_M[2, 3],
                  wh_M[3, 1] + wh_M[3, 2] + wh_M[3, 3]};

      // --- hand non-gravity wrench to the rigid-body integrator ---
      F_b = F_aero + F_thrust + F_ground;
      M_b = M_aero + M_ground;

      // --- viewer outputs ---
      gyro = omega;
      up_body = R[3, :];
      airspeed = Vt;
      alpha_deg = alpha*57.29577951308232;
      position = p;
      velocity = v_w;
      quat = q;
    end FixedWingPlant;


    model FixedWingFBW "Cascaded attitude-hold fly-by-wire (outer attitude -> inner rate)"
      // OUTER loop — the stick commands an attitude RATE that integrates into a
      // held bank / pitch setpoint (rate-command, attitude-hold). Holding a key
      // keeps rolling / pitching; releasing holds the new attitude. A proportional
      // law on the setpoint error produces body-rate setpoints for the inner loop.
      parameter Real roll_cmd_rate = 1.5 "Full-stick roll-setpoint rate [rad/s]";
      parameter Real pitch_cmd_rate = 0.9 "Full-stick pitch-setpoint rate [rad/s]";
      parameter Real phi_sp_max = 0.90 "Bank setpoint limit [rad] (~52 deg)";
      parameter Real theta_sp_max = 0.45 "Pitch setpoint limit [rad] (~26 deg)";
      parameter Real yaw_rate_max = 1.0 "Full-stick commanded yaw rate [rad/s]";
      parameter Real Kp_phi = 5.0 "Bank-error -> roll-rate gain [1/s]";
      parameter Real Kp_theta = 5.0 "Pitch-error -> pitch-rate gain [1/s]";
      parameter Real p_rate_max = 4.0 "Roll-rate setpoint limit [rad/s]";
      parameter Real q_rate_max = 2.5 "Pitch-rate setpoint limit [rad/s]";

      // INNER loop — body-rate PI (anti-windup). The integral supplies the trim
      // surface deflection, so attitude is held with no steady-state droop.
      parameter Real Kp_p = 0.45 "Roll-rate P [cmd/(rad/s)]";
      parameter Real Ki_p = 0.30 "Roll-rate I";
      parameter Real ilim_p = 1.0 "Roll integrator limit";
      parameter Real Kp_q = 0.55 "Pitch-rate P";
      parameter Real Ki_q = 0.40 "Pitch-rate I";
      parameter Real ilim_q = 1.0 "Pitch integrator limit";
      parameter Real Kp_r = 0.40 "Yaw-rate P";
      parameter Real Ki_r = 0.10 "Yaw-rate I";
      parameter Real ilim_r = 0.6 "Yaw integrator limit";

      // Stall protection — nose-up authority fades out as airspeed approaches the
      // stall, and below the floor the controller actively commands nose-down to
      // trade altitude for speed, so the aircraft sinks rather than departing.
      parameter Real v_prot_lo = 6.5 "Airspeed below which nose-up authority -> 0 [m/s]";
      parameter Real v_prot_hi = 9.0 "Airspeed for full nose-up authority [m/s]";
      parameter Real dive_slope = 0.10 "Protective nose-down angle per m/s of speed deficit [rad/(m/s)]";

      input Real stick_roll(start = 0) "Roll stick [-1..1]";
      input Real stick_pitch(start = 0) "Pitch stick [-1..1]";
      input Real stick_yaw(start = 0) "Yaw stick [-1..1]";
      input Real stick_throttle(start = 0) "Throttle stick [0..1]";
      input Real armed(start = 0) "Arm signal [0/1]";
      input Real gyro[3](start = {0, 0, 0}) "Body rate FLU [rad/s]";
      input Real up_body[3](start = {0, 0, 1}) "World up in body FLU";
      input Real airspeed(start = 15) "True airspeed [m/s]";

      output Real ail "Aileron command [-1..1]";
      output Real elev "Elevator command [-1..1]";
      output Real rud "Rudder command [-1..1]";
      output Real thr "Throttle command [0..1]";

    protected
      Real phi "Estimated bank angle [rad]";
      Real theta "Estimated pitch angle [rad]";
      Real p_sp "Roll-rate setpoint [rad/s]";
      Real q_up_sp "Nose-up-rate setpoint [rad/s]";
      Real r_sp "Yaw-rate setpoint [rad/s]";
      Real p_meas "Measured roll rate [rad/s]";
      Real q_up "Measured nose-up rate [rad/s]";
      Real r_meas "Measured yaw rate [rad/s]";
      Real e_p, e_q, e_r "Body-rate errors [rad/s]";
      Real i_p(start = 0, fixed = true);
      Real i_q(start = 0, fixed = true);
      Real i_r(start = 0, fixed = true);
      Real i_p_c, i_q_c, i_r_c "Clamped integrator states";
      Real phi_sp(start = 0, fixed = true) "Held bank setpoint [rad]";
      Real theta_sp(start = 0, fixed = true) "Held pitch setpoint [rad]";
      Real rate_phi "Commanded roll-setpoint rate [rad/s]";
      Real rate_theta "Commanded pitch-setpoint rate [rad/s]";
      Real climb_auth "Nose-up authority [0..1], faded near stall";
      Real theta_eff "Pitch setpoint after protective dive [rad]";

    equation
      // --- attitude estimate from world-up-in-body (tilt angles, no singularity
      //     for normal flight). up_body = {fwd, left, up}; level -> {0, 0, 1}.
      phi = atan2(up_body[2], up_body[3]);
      theta = atan2(up_body[1], up_body[3]);

      // --- OUTER loop: rate-command into held setpoints, then setpoint error ->
      //     body-rate setpoints. Nose-up rate fades near stall (so the setpoint
      //     can't wind up nose-high); a protective dive overrides the pitch
      //     setpoint when slow and releases itself once speed recovers.
      climb_auth = min(1, max(0, (airspeed - v_prot_lo)/(v_prot_hi - v_prot_lo)));
      rate_phi = armed*stick_roll*roll_cmd_rate;
      rate_theta = armed*(if stick_pitch > 0 then stick_pitch*pitch_cmd_rate*climb_auth else stick_pitch*pitch_cmd_rate);
      der(phi_sp) = if phi_sp > phi_sp_max then min(0, rate_phi) else if phi_sp < -phi_sp_max then max(0, rate_phi) else rate_phi;
      der(theta_sp) = if theta_sp > theta_sp_max then min(0, rate_theta) else if theta_sp < -theta_sp_max then max(0, rate_theta) else rate_theta;

      theta_eff = if airspeed < v_prot_lo then min(theta_sp, -dive_slope*(v_prot_lo - airspeed)) else theta_sp;

      p_sp = min(p_rate_max, max(-p_rate_max, Kp_phi*(phi_sp - phi)));
      q_up_sp = min(q_rate_max, max(-q_rate_max, Kp_theta*(theta_eff - theta)));
      r_sp = stick_yaw*yaw_rate_max;

      // --- measured rates in matching sign convention (measured open-loop):
      //     +aileron -> roll-right -> gyro[1] > 0;  +elevator -> nose-up ->
      //     gyro[2] < 0 (so nose-up rate q_up = -gyro[2]);  +rudder -> gyro[3] > 0.
      p_meas = gyro[1];
      q_up = -gyro[2];
      r_meas = gyro[3];

      e_p = p_sp - p_meas;
      e_q = q_up_sp - q_up;
      e_r = r_sp - r_meas;

      // --- INNER loop: rate PI with conditional anti-windup, gated by arm ---
      der(i_p) = if i_p > ilim_p then min(0, armed*e_p) else if i_p < -ilim_p then max(0, armed*e_p) else armed*e_p;
      der(i_q) = if i_q > ilim_q then min(0, armed*e_q) else if i_q < -ilim_q then max(0, armed*e_q) else armed*e_q;
      der(i_r) = if i_r > ilim_r then min(0, armed*e_r) else if i_r < -ilim_r then max(0, armed*e_r) else armed*e_r;
      i_p_c = min(ilim_p, max(-ilim_p, i_p));
      i_q_c = min(ilim_q, max(-ilim_q, i_q));
      i_r_c = min(ilim_r, max(-ilim_r, i_r));

      ail = Kp_p*e_p + Ki_p*i_p_c;
      elev = Kp_q*e_q + Ki_q*i_q_c;
      rud = Kp_r*e_r + Ki_r*i_r_c;

      // --- manual throttle, gated by arm ---
      thr = armed*stick_throttle;
    end FixedWingFBW;
  end Aircraft;

  model ResettablePlant "FixedWingPlant whose rigid-body states return to the start on reset"
    extends Aircraft.FixedWingPlant;
    input Boolean reset;
  equation
    when reset then
      reinit(p, p_start);
      reinit(v_b, v_b_start);
      reinit(q, q_start);
      reinit(omega, omega_start);
    end when;
  end ResettablePlant;

  model ResettableFBW "FixedWingFBW whose setpoints and integrators clear on reset"
    extends Aircraft.FixedWingFBW;
    input Boolean reset;
    output Real bankSetpoint = phi_sp "Held bank setpoint [rad]";
    output Real pitchSetpoint = theta_sp "Held pitch setpoint [rad]";
  equation
    when reset then
      reinit(phi_sp, 0);
      reinit(theta_sp, 0);
      reinit(i_p, 0);
      reinit(i_q, 0);
      reinit(i_r, 0);
    end when;
  end ResettableFBW;

  model Canyon "Builds the observation: 15 true-state features and the local occupancy grid"
    parameter Integer nRows = 8 "Occupancy grid rows" annotation(Evaluate = true);
    parameter Integer nCols = 8 "Occupancy grid columns" annotation(Evaluate = true);
    parameter Real dt = 0.1 "Observation period [s]";
    final parameter Integer nCells = nRows*nCols;
    final parameter Integer nObs = 15 + nCells;
    input Real position[3];
    input Real quat[4];
    input Real velocity[3];
    input Real gyro[3];
    input Real setpoints[2] "Fly-by-wire held bank and pitch setpoints";
    output Real obs[nObs](each start = 0, each fixed = true);
  equation
    when sample(0, dt) then
      obs = cat(1, Course.features(position, quat, velocity, gyro, setpoints),
        Course.occupancy(position, quat, nRows, nCols));
    end when;
  end Canyon;

  model Episode "Reward, crash / finish / timeout and the episode counters"
    parameter Real dt = 0.1 "Sample period [s]";
    parameter Real maxTime = 60 "Episode timeout [s]";
    parameter Real progressReward = 0.1 "Reward per metre flown down the canyon";
    parameter Real crashPenalty = 5;
    parameter Real finishBonus = 5;
    parameter Real centringPenalty = 0.05 "Per-sample penalty at the wall, scaled by (offset/half-width)^2";
    parameter Real minAirspeed = 4 "Below this the flight counts as a crash [m/s]";
    input Real position[3];
    input Real airspeed;
    output Real reward(start = 0, fixed = true);
    output Boolean crashed(start = false, fixed = true);
    output Boolean finished(start = false, fixed = true);
    output Boolean truncated(start = false, fixed = true) "Timeout: the episode ends but is not a failure";
    output Boolean reset(start = false, fixed = true) "The episode ends at this sample";
    output Integer episode(start = 1, fixed = true);
    output Real episodeReturn(start = 0, fixed = true) "Return so far in this episode";
    output Real lastReturn(start = 0, fixed = true) "Return of the last finished episode";
    output Real lastDistance(start = 0, fixed = true) "Distance flown in the last finished episode [m]";
    output Integer lastCause(start = 0, fixed = true)
      "How the last episode ended: 1 wall, 2 floor, 3 over the rim, 4 too slow, 5 finish, 6 timeout";
  protected
    discrete Real xPrev(start = Course.startX, fixed = true) "Position along the canyon at the previous sample";
    discrete Real tStart(start = 0, fixed = true) "Start time of this episode";
  algorithm
    when sample(0, dt) then
      crashed := not Course.inFreeAir(position[1], position[2], position[3])
        or position[3] > Course.rimHeight or airspeed < minAirspeed;
      finished := not crashed and position[1] >= Course.length;
      truncated := not crashed and not finished and time - pre(tStart) >= maxTime;
      reset := crashed or finished or truncated;
      reward := progressReward*(position[1] - pre(xPrev))
        - centringPenalty*((position[2] - Course.centreLine(position[1]))/Course.halfWidth(position[1]))^2
        - (if crashed then crashPenalty else 0) + (if finished then finishBonus else 0);
      episodeReturn := (if pre(reset) then 0 else pre(episodeReturn)) + reward;
      lastReturn := if reset then episodeReturn else pre(lastReturn);
      lastDistance := if reset then position[1] - Course.startX else pre(lastDistance);
      lastCause := if not reset then pre(lastCause)
        elseif finished then 5 elseif truncated then 6
        elseif position[3] <= 0 then 2 elseif position[3] > Course.rimHeight then 3
        elseif airspeed < minAirspeed then 4 else 1;
      episode := pre(episode) + (if pre(reset) then 1 else 0);
      xPrev := if reset then Course.startX else position[1];
      tStart := if reset then time else pre(tStart);
    end when;
  end Episode;

  model Agent "Deep Q-learning: remember, act, learn, all in one sampled algorithm"
    parameter Integer nObs = 79 "Observation size" annotation(Evaluate = true);
    parameter Integer nHidden1 = 32 "First hidden layer width" annotation(Evaluate = true);
    parameter Integer nHidden2 = 16 "Second hidden layer width" annotation(Evaluate = true);
    parameter Integer nAct = 5 "Number of actions" annotation(Evaluate = true);
    parameter Integer ringSize = 64 "Replay ring length [transitions]" annotation(Evaluate = true);
    parameter Integer batchSize = 16 "Transitions per learning step" annotation(Evaluate = true);
    parameter Real dt = 0.1 "Agent period [s]";
    parameter Real gamma = 0.97 "Discount per step";
    parameter Real learningRate = 1e-3;
    parameter Real epsStart = 1.0 "Exploration at the first step";
    parameter Real epsEnd = 0.05 "Exploration after the decay";
    parameter Integer epsDecaySteps = 8000 "Steps over which exploration decays";
    parameter Integer targetSyncSteps = 250 "Learning steps between target-network copies";
    parameter Integer defaultAction = 1 "Action used on the first sample of an episode: level flight";
    parameter Integer seed = 1 "Exploration and replay stream";

    input Real obs[nObs];
    input Real reward "Reward for the previous action";
    input Boolean terminal "The previous action ended the episode in failure or at the finish";
    input Boolean truncated "The episode timed out at this sample";
    output Integer action(start = defaultAction, fixed = true) "Action, 1..nAct";
    output Real epsilon(start = epsStart, fixed = true) "Current exploration probability";
    output Real qMax(start = 0, fixed = true) "Largest Q-value at the current observation";
    output Integer steps(start = 0, fixed = true) "Agent samples so far";
    output Integer updates(start = 0, fixed = true) "Learning steps so far";

    discrete Real W1[nHidden1, nObs](start = Net.initWeights(nHidden1, nObs, 1), each fixed = true);
    discrete Real b1[nHidden1](each start = 0, each fixed = true);
    discrete Real W2[nHidden2, nHidden1](start = Net.initWeights(nHidden2, nHidden1, 2), each fixed = true);
    discrete Real b2[nHidden2](each start = 0, each fixed = true);
    discrete Real W3[nAct, nHidden2](start = 0.1*Net.initWeights(nAct, nHidden2, 3), each fixed = true);
    discrete Real b3[nAct](each start = 0, each fixed = true);

  protected
    discrete Real T1[nHidden1, nObs](start = Net.initWeights(nHidden1, nObs, 1), each fixed = true) "Target network";
    discrete Real c1[nHidden1](each start = 0, each fixed = true);
    discrete Real T2[nHidden2, nHidden1](start = Net.initWeights(nHidden2, nHidden1, 2), each fixed = true);
    discrete Real c2[nHidden2](each start = 0, each fixed = true);
    discrete Real T3[nAct, nHidden2](start = 0.1*Net.initWeights(nAct, nHidden2, 3), each fixed = true);
    discrete Real c3[nAct](each start = 0, each fixed = true);

    discrete Real mW1[nHidden1, nObs](each start = 0, each fixed = true) "Adam first moments";
    discrete Real mb1[nHidden1](each start = 0, each fixed = true);
    discrete Real mW2[nHidden2, nHidden1](each start = 0, each fixed = true);
    discrete Real mb2[nHidden2](each start = 0, each fixed = true);
    discrete Real mW3[nAct, nHidden2](each start = 0, each fixed = true);
    discrete Real mb3[nAct](each start = 0, each fixed = true);
    discrete Real vW1[nHidden1, nObs](each start = 0, each fixed = true) "Adam second moments";
    discrete Real vb1[nHidden1](each start = 0, each fixed = true);
    discrete Real vW2[nHidden2, nHidden1](each start = 0, each fixed = true);
    discrete Real vb2[nHidden2](each start = 0, each fixed = true);
    discrete Real vW3[nAct, nHidden2](each start = 0, each fixed = true);
    discrete Real vb3[nAct](each start = 0, each fixed = true);

    discrete Real ringObs[ringSize, nObs](each start = 0, each fixed = true) "Replay ring";
    discrete Real ringNext[ringSize, nObs](each start = 0, each fixed = true);
    discrete Integer ringAct[ringSize](each start = 1, each fixed = true);
    discrete Real ringReward[ringSize](each start = 0, each fixed = true);
    discrete Boolean ringDone[ringSize](each start = false, each fixed = true);
    discrete Integer stored(start = 0, fixed = true) "Transitions written so far";

    discrete Real prevObs[nObs](each start = 0, each fixed = true);
    discrete Boolean prevValid(start = false, fixed = true);

    discrete Boolean learning(start = false, fixed = true) "The ring holds a batch, so each sample takes an Adam step";
    Real gW1[nHidden1, nObs] "Batch gradients";
    Real gb1[nHidden1];
    Real gW2[nHidden2, nHidden1];
    Real gb2[nHidden2];
    Real gW3[nAct, nHidden2];
    Real gb3[nAct];
    Real q[nAct];

  algorithm
    // pre(x) is the value x held before this sample; a plain name is its value
    // after the line that updates it. The block reads top to bottom like the
    // textbook loop.
    when sample(0, dt) then
      // 1. Remember: the previous observation, the action taken there, the
      //    reward it earned and where it led.
      (ringObs, ringAct, ringReward, ringNext, ringDone, stored) := Net.remember(
        pre(ringObs), pre(ringAct), pre(ringReward), pre(ringNext), pre(ringDone), pre(stored),
        pre(prevValid), pre(prevObs), pre(action), reward, obs, terminal);

      // 2. Act: epsilon-greedy with the network as it stood at the start of
      //    this sample. The first sample of a new episode flies level at cruise.
      epsilon := max(epsEnd, epsStart - (epsStart - epsEnd)*pre(steps)/epsDecaySteps);
      q := Net.forward(pre(W1), pre(b1), pre(W2), pre(b2), pre(W3), pre(b3), obs);
      qMax := q[Net.argmax(q)];
      action := Net.chooseAction(q, epsilon, pre(steps), 2*seed, terminal or truncated, defaultAction);
      prevObs := obs;
      prevValid := not (terminal or truncated);

      // 3. Learn: one Adam step on a batch drawn from the ring as it stood at
      //    the start of this sample, towards r + gamma * max_a' Q_target(s', a').
      //    The transition stored above joins the draws from the next sample on.
      learning := min(pre(stored), ringSize) >= batchSize;
      (gW1, gb1, gW2, gb2, gW3, gb3) := Net.batchGradient(
        pre(W1), pre(b1), pre(W2), pre(b2), pre(W3), pre(b3),
        pre(T1), pre(c1), pre(T2), pre(c2), pre(T3), pre(c3),
        pre(ringObs), pre(ringAct), pre(ringReward), pre(ringNext), pre(ringDone),
        min(pre(stored), ringSize), batchSize, pre(steps), 2*seed + 1, gamma);
      updates := pre(updates) + (if learning then 1 else 0);
      (W1, mW1, vW1) := Net.adamStep(pre(W1), gW1, pre(mW1), pre(vW1), max(1, updates), learningRate);
      (b1, mb1, vb1) := Net.adamStepVector(pre(b1), gb1, pre(mb1), pre(vb1), max(1, updates), learningRate);
      (W2, mW2, vW2) := Net.adamStep(pre(W2), gW2, pre(mW2), pre(vW2), max(1, updates), learningRate);
      (b2, mb2, vb2) := Net.adamStepVector(pre(b2), gb2, pre(mb2), pre(vb2), max(1, updates), learningRate);
      (W3, mW3, vW3) := Net.adamStep(pre(W3), gW3, pre(mW3), pre(vW3), max(1, updates), learningRate);
      (b3, mb3, vb3) := Net.adamStepVector(pre(b3), gb3, pre(mb3), pre(vb3), max(1, updates), learningRate);

      // Every targetSyncSteps learning steps, copy the network to the target.
      T1 := Net.copyIf(learning and mod(updates, targetSyncSteps) == 0, W1, pre(T1));
      c1 := Net.copyVectorIf(learning and mod(updates, targetSyncSteps) == 0, b1, pre(c1));
      T2 := Net.copyIf(learning and mod(updates, targetSyncSteps) == 0, W2, pre(T2));
      c2 := Net.copyVectorIf(learning and mod(updates, targetSyncSteps) == 0, b2, pre(c2));
      T3 := Net.copyIf(learning and mod(updates, targetSyncSteps) == 0, W3, pre(T3));
      c3 := Net.copyVectorIf(learning and mod(updates, targetSyncSteps) == 0, b3, pre(c3));
      steps := pre(steps) + 1;
    end when;
  end Agent;

  function stickRoll "Roll stick of an action: level, bank left, bank right, pitch up, pitch down"
    input Integer action;
    input Integer nThrottle "Throttle levels per manoeuvre: 1 (cruise only) or 3";
    output Real stick;
  protected
    Integer manoeuvre;
  algorithm
    manoeuvre := integer((action - 1)/nThrottle) + 1;
    stick := if manoeuvre == 2 then -1 elseif manoeuvre == 3 then 1 else 0;
  end stickRoll;

  function stickPitch "Pitch stick of an action"
    input Integer action;
    input Integer nThrottle;
    output Real stick;
  protected
    Integer manoeuvre;
  algorithm
    manoeuvre := integer((action - 1)/nThrottle) + 1;
    stick := if manoeuvre == 4 then 1 elseif manoeuvre == 5 then -1 else 0;
  end stickPitch;

  function throttle "Throttle of an action: cruise, or low / cruise / high with three levels"
    input Integer action;
    input Integer nThrottle;
    output Real thr;
  protected
    Integer level;
  algorithm
    level := mod(action - 1, nThrottle) + 1;
    thr := if nThrottle == 1 then 0.25 elseif level == 1 then 0.15 elseif level == 2 then 0.25 else 0.4;
  end throttle;

  model Run "The aircraft, the canyon and the learner, wired together"
    parameter Real dt = 0.1 "Agent and sensor period [s]";
    parameter Integer nRows = 8 "Occupancy grid rows" annotation(Evaluate = true);
    parameter Integer nCols = 8 "Occupancy grid columns" annotation(Evaluate = true);
    parameter Integer nHidden1 = 32 "First hidden layer width" annotation(Evaluate = true);
    parameter Integer nHidden2 = 16 "Second hidden layer width" annotation(Evaluate = true);
    parameter Integer ringSize = 64 "Replay ring length [transitions]" annotation(Evaluate = true);
    parameter Integer batchSize = 16 "Transitions per learning step" annotation(Evaluate = true);
    parameter Integer seed = 1 "Exploration and replay stream";
    parameter Integer epsDecaySteps = 8000 "Agent steps over which exploration decays";
    parameter Integer nThrottle = 1 "Throttle levels per manoeuvre: 1 (cruise only) or 3"
      annotation(Evaluate = true);

    output Real position[3] "World position [m]";
    output Real quat[4] "Quaternion w,x,y,z";
    output Real airspeed "True airspeed [m/s]";
    output Integer action "Current action";
    output Integer episode "Episode number";
    output Real episodeReturn "Return so far in this episode";
    output Real lastReturn "Return of the last finished episode";
    output Real lastDistance "Distance flown in the last finished episode [m]";
    output Real epsilon "Exploration probability";

    ResettablePlant vehicle(
      p_start = {Course.startX, Course.centreLine(Course.startX), Course.startHeight},
      v_b_start = {Course.startSpeed, 0, 0},
      q_start = Course.startAttitude());
    ResettableFBW controller;
    Canyon canyon(dt = dt, nRows = nRows, nCols = nCols);
    Episode episodeLogic(dt = dt);
    Agent agent(dt = dt, nObs = canyon.nObs, nHidden1 = nHidden1, nHidden2 = nHidden2,
      ringSize = ringSize, batchSize = batchSize, seed = seed, epsDecaySteps = epsDecaySteps,
      nAct = 5*nThrottle, defaultAction = if nThrottle == 3 then 2 else 1);

  equation
    controller.stick_roll = stickRoll(agent.action, nThrottle);
    controller.stick_pitch = stickPitch(agent.action, nThrottle);
    controller.stick_yaw = 0;
    controller.stick_throttle = throttle(agent.action, nThrottle);
    controller.armed = 1;
    controller.gyro = vehicle.gyro;
    controller.up_body = vehicle.up_body;
    controller.airspeed = vehicle.airspeed;
    vehicle.ail = controller.ail;
    vehicle.elev = controller.elev;
    vehicle.rud = controller.rud;
    vehicle.thr = controller.thr;

    canyon.position = vehicle.position;
    canyon.quat = vehicle.quat;
    canyon.velocity = vehicle.velocity;
    canyon.gyro = vehicle.gyro;
    canyon.setpoints = {controller.bankSetpoint, controller.pitchSetpoint};
    episodeLogic.position = vehicle.position;
    episodeLogic.airspeed = vehicle.airspeed;
    agent.obs = canyon.obs;
    agent.reward = episodeLogic.reward;
    agent.terminal = episodeLogic.crashed or episodeLogic.finished;
    agent.truncated = episodeLogic.truncated;
    vehicle.reset = episodeLogic.reset;
    controller.reset = episodeLogic.reset;

    position = vehicle.position;
    quat = vehicle.quat;
    airspeed = vehicle.airspeed;
    action = agent.action;
    episode = episodeLogic.episode;
    episodeReturn = episodeLogic.episodeReturn;
    lastReturn = episodeLogic.lastReturn;
    lastDistance = episodeLogic.lastDistance;
    epsilon = agent.epsilon;
    annotation(experiment(StopTime = 600, Interval = 0.01, Solver = "rk-like"));
  end Run;

  model Smoke "Fixed short run for checks: about 200 agent samples from the same start"
    extends Run;
    annotation(experiment(StopTime = 20, Interval = 0.01, Solver = "rk-like"));
  end Smoke;
end CanyonRun;
