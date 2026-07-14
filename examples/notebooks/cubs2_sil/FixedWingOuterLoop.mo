// SPDX-License-Identifier: Apache-2.0
//
// Fixed-wing outer-loop autopilot for the HobbyZone Sport Cub S2.
//
// This fixed-period sampled model is the source for Rumoca eFMI Production Code.
// Keep helper functions and controller blocks in this file so generated code can
// be traced back to one inspectable control model.

function clip
  input Real value;
  input Real lower;
  input Real upper;
  output Real result;
algorithm
  result := min(max(value, lower), upper);
annotation(
  Inline = true);
end clip;

function wrapAngle
  input Real angle(unit = "rad");
  output Real result(unit = "rad");
algorithm
  // atan2(sin(theta), cos(theta)) keeps heading/pitch errors continuous across +/-pi.
  result := atan2(sin(angle), cos(angle));
annotation(
  Inline = true);
end wrapAngle;

function lowPass
  input Real sample[3];
  input Real previous[3];
  input Real sampleWeight;
  output Real result[3];
algorithm
  result := sampleWeight * sample + (1.0 - sampleWeight) * previous;
annotation(
  Inline = true);
end lowPass;

function lowPassScalar
  input Real sample;
  input Real previous;
  input Real sampleWeight;
  output Real result;
algorithm
  result := sampleWeight * sample + (1.0 - sampleWeight) * previous;
annotation(
  Inline = true);
end lowPassScalar;

function rateLimit
  input Real target;
  input Real current;
  input Real maxStep;
  output Real result;
algorithm
  result := current + clip(target - current, -maxStep, maxStep);
annotation(
  Inline = true);
end rateLimit;

function vectorNorm2
  input Real v[2];
  output Real result;
algorithm
  result := sqrt(v * v);
annotation(
  Inline = true);
end vectorNorm2;

function vectorNorm3
  input Real v[3];
  output Real result;
algorithm
  result := sqrt(v * v);
annotation(
  Inline = true);
end vectorNorm3;

function waypointAt
  input Real waypoints[7, 3];
  input Integer index;
  output Real waypoint[3];
algorithm
  waypoint := {waypoints[index, 1], waypoints[index, 2], waypoints[index, 3]};
annotation(
  Inline = true);
end waypointAt;

function horizontalPart
  input Real v[3];
  output Real result[2];
algorithm
  result := {v[1], v[2]};
annotation(
  Inline = true);
end horizontalPart;

function horizontalDisplacement
  input Real position[3];
  input Real origin[3];
  output Real result[2];
algorithm
  result := {position[1] - origin[1], position[2] - origin[2]};
annotation(
  Inline = true);
end horizontalDisplacement;

record VehicleParameters
  Real g(unit = "m/s2") = 9.81 "standard gravity";
  Real mass(unit = "kg") = 0.063 "flight-tuned 2026-07-10; SportCubPlant sims at 0.065";
  Real thrustMax(unit = "N") = 0.30 "FixedWingPlant.thr_max";
  Real trimThrust(unit = "N") = 0.12 "40% throttle cruise trim (2026-07-10)";
  Real envelopeDrag(unit = "N") = 0.07 "cruise drag";
  Real weight(unit = "N") = mass * g "aircraft weight";
  Real drag(unit = "N") = envelopeDrag "drag estimate";
end VehicleParameters;

record PidParameters
  Real dt(unit = "s") = 0.02;
  Boolean useInputDerivative = true;
  Real trim = 0.0;
  Real kp = 0.0;
  Real ki = 0.0;
  Real kd = 0.0;
  Real integralMax = 1.0;
  Real commandMin = -1.0;
  Real commandMax = 1.0;
end PidParameters;

record FlightState
  Real position_m[3] = {0.0, 0.0, 0.0};
  Real euler_rad[3] = {0.0, 0.0, 0.0};
  Real velocity_m_s[3] = {0.0, 0.0, 0.0};
  Real speed(unit = "m/s") = 0.0;
  Real flightPathAngle(unit = "rad") = 0.0;
  Real acceleration_m_s2(unit = "m/s2") = 0.0;
  Real eulerRate_rad_s[3] = {0.0, 0.0, 0.0};
end FlightState;

record GuidanceSetpoints
  Real speed(unit = "m/s") = 0.0;
  Real flightPathAngle(unit = "rad") = 0.0;
  Real heading(unit = "rad") = 0.0;
  Real acceleration(unit = "m/s2") = 0.0;
end GuidanceSetpoints;

record RouteParameters
  Integer nSegments = 6 "flyable segments between route points";
  Real waypoints[7, 3] = [
    0.0,   0.0, 0.0;
    -8.0, -8.0, 3.0;
    -8.0,  2.0, 3.0;
    18.0,  2.0, 3.0;
    18.0, -8.0, 3.0;
    5.0,  -8.0, 3.0;
    -8.0, -8.0, 3.0] "route point rows are [x, y, z] [m]";
  Real cruiseSpeed(unit = "m/s") = 4.5;
  Real altitudeToFlightPathGain = 2.0;
  Real altitudeLookaheadDistance(unit = "m") = 8.0;
  Real flightPathAngleLimit(unit = "rad") = 0.12;
  Real speedToAccelerationGain = 1.0;
  Real crossTrackSteeringDistance(unit = "m") = 4.25
    "atan steering distance; 45 deg correction occurs when |cross-track| = d";
  Real waypointSwitchingDistance(unit = "m") = 4.0
    "advance when remaining along-track distance enters the endpoint guard";
end RouteParameters;

record TecsParameters
  Real thrustKp = 0.05 "P on energy-rate error";
  Real thrustKi = 0.004854133294500267
    "10 s unit-error ramp to 10% throttle integral authority";
  Real energyRateIntegralMax = 10.0
    "caps thrust integral at +/-10% throttle";
  Real pitchKp = 0.075;
  Real pitchKi = 0.0;
  Real energyDistributionIntegralMax = 0.0;
  Real pitchCommandLimit(unit = "rad") = 0.20943951023931953;
  Real turnThrustGain = 1.0 "thrust FF per unit load-factor excess";
  Real turnPitchGain = 0.0 "pitch FF [rad] per unit load-factor excess";
end TecsParameters;

record AttitudeParameters
  Real takeoffAltitude(unit = "m") = 0.4;
  Real takeoffElevator = 0.15;
  Real trimElevator = 0.0
    "manual cruise trim from 2026-07-09/10 level-ish flight segments";
  Real stabilizerCommand(unit = "us") = 2000.0;
  Real pitchCommandToElevatorGain = 1.0 / 0.5235987755982988
    "S2 pitch stick gain: full elevator stick maps to 30 deg (bench-measured 2026-07-10)";
  // Full aileron stick is 45 deg of bank on the real S2.
  Real rollCommandToAileronGain = 1.0 / 0.7853981633974483
    "S2 bank stick gain: full aileron stick maps to 45 deg";
  Real rollLimit(unit = "rad") = 0.5235987755982988
    "30 deg bank command saturation";
  Real rollRateLimit(unit = "rad/s") = 2.0943951023931953;
  Real courseDeadband(unit = "rad") = 0.03490658503988659;
  Real groundedResetTime(unit = "s") = 2.0
    "continuous time below 0.75*takeoffAltitude that re-arms takeoff";
  Real staleFailsafeTime(unit = "s") = 0.7
    "pose-frozen time that triggers neutral-stick failsafe (SAFE levels wings)";
  Real staleFailsafeThrottle = 0.3;
  Real climboutAltitude(unit = "m") = 1.2
    "hand off to TECS above this; below it fly a fixed-pitch climb";
  Real climboutPitch(unit = "rad") = 0.17
    "10 deg: at the power-limited climb ceiling; 15 deg stalled (17-10-43)";
  Real climboutSpeedGain(unit = "rad.s/m") = 0.12
    "zoom: extra pitch per m/s above cruise, spent as the excess bleeds off";
  Real climboutMaxPitch(unit = "rad") = 0.44;
  Real climboutMaxElevator = 0.6
    "up-elevator ceiling during climbout: the entry transient must not slam full-up into a stall";
  Real climboutMinElevator = -0.2
    "down-elevator floor during climbout (a takeoff balloon must not slam nose-down near the ground)";
  Real climboutRollLimit(unit = "rad") = 0.26
    "gentle guidance steering while climbing keeps the run off the walls";
  Real handoffGuardTime(unit = "s") = 0.0
    "0 = full authority at handoff (240 Hz feed restores honest speed estimates)";
  Real handoffMinThrottle = 0.35;
end AttitudeParameters;

block PidController
  parameter PidParameters params = PidParameters();

  discrete Boolean enabled(start = false);
  discrete Real error(start = 0.0);
  discrete Real derivativeInput(start = 0.0);
  discrete Real feedforward(start = 0.0);

  discrete output Real derivative(start = 0.0);
  discrete output Real integral(start = 0.0);
  discrete output Real command(start = 0.0);
  discrete output Real preview(start = 0.0)
    "integral-free command; what auto would fly while disabled";

protected
  discrete Real previousError(start = 0.0);

algorithm
  when sample(0.0, params.dt) then
    derivative :=
      if params.useInputDerivative then
        derivativeInput
      else
        (error - pre(previousError)) / params.dt;
    preview :=
      clip(params.trim
           + params.kp * error
           + params.kd * derivative
           + feedforward,
           params.commandMin,
           params.commandMax);
    if enabled then
      command :=
        clip(params.trim
             + params.kp * error
             + params.ki * pre(integral)
             + params.kd * derivative
             + feedforward,
             params.commandMin,
             params.commandMax);
      // Anti-windup: freeze the integral while the command is saturated in
      // the error's direction (same policy as the TECS integrators).
      if not ((command >= params.commandMax - 1e-9 and error > 0.0)
              or (command <= params.commandMin + 1e-9 and error < 0.0)) then
        integral :=
          clip(pre(integral) + error * params.dt,
               -params.integralMax,
               params.integralMax);
      else
        integral := pre(integral);
      end if;
    else
      integral := 0.0;
      command := params.trim;
    end if;
    previousError := error;
  end when;
end PidController;

block StateEstimator
  parameter Real dt(unit = "s") = 0.02;
  parameter Real filterCutoffHz(unit = "Hz") = 10.0;
  // Lower cutoff for the energy path: pose arrival jitter puts +/-50% error
  // on per-sample velocity, which is zero-mean and averages out.
  parameter Real velocityFilterCutoffHz(unit = "Hz") = 1.5;
  parameter Real accelFilterCutoffHz(unit = "Hz") = 0.4;
  parameter Real jumpSpeedLimit(unit = "m/s") = 10.0
    "implied speed above this marks a pose sample as a glitch";
  parameter Real jumpAcceptTime(unit = "s") = 0.5
    "sustained offset longer than this is a reacquire: re-anchor the pose";
  parameter Real useMeasuredRates(min = 0.0, max = 1.0) = 1.0
    "1: velocity/rate inputs are trusted measurements; 0: derive rates from pose only";
  constant Real pi = 3.141592653589793;
  constant Real zero3[3] = {0.0, 0.0, 0.0};

  input Real position_m[3];
  input Real euler_rad[3];
  input Real velocity_m_s[3];
  input Real eulerRate_rad_s[3];
  discrete output FlightState estimate = FlightState();
  discrete output Integer heldSamples(start = 0)
    "consecutive cycles without an accepted fresh pose";

protected
  discrete Boolean started(start = false);
  discrete Real previousPosition_m[3](each start = 0.0);
  discrete Real previousEuler_rad[3](each start = 0.0);
  discrete Real previousFilteredSpeed(start = 0.0);
  discrete Real rawVelocity_m_s[3];
  discrete Real rawEulerRate_rad_s[3];
  discrete Real wrappedEulerDelta_rad[3];
  discrete Real filteredEulerDelta_rad[3];
  discrete Real measuredSpeed;
  discrete Real measuredFlightPathAngle;
  discrete Real filterSampleWeight;
  discrete Real velocitySampleWeight;
  discrete Real accelSampleWeight;
  discrete Real rawAcceleration_m_s2;
  discrete Real sampleGap_s;
  discrete Real positionDelta_m[3];
  discrete Real impliedSpeed_m_s;

algorithm
  when sample(0.0, dt) then
    // The continuous-time pole maps to the previous-sample weight; lowPass
    // takes the complementary new-sample weight.
    filterSampleWeight := 1.0 - exp(-2.0 * pi * filterCutoffHz * dt);
    velocitySampleWeight := 1.0 - exp(-2.0 * pi * velocityFilterCutoffHz * dt);
    accelSampleWeight := 1.0 - exp(-2.0 * pi * accelFilterCutoffHz * dt);

    if not pre(started) then
      estimate.position_m := position_m;
      estimate.euler_rad := euler_rad;
      estimate.velocity_m_s := velocity_m_s;
      estimate.eulerRate_rad_s := eulerRate_rad_s;
      estimate.speed := vectorNorm3(velocity_m_s);
      estimate.flightPathAngle :=
        asin(clip(velocity_m_s[3] / max(estimate.speed, 1e-5), -1.0, 1.0));
      estimate.acceleration_m_s2 := 0.0;
      started := true;
      heldSamples := 0;
    elseif useMeasuredRates > 0.5 then
      for i in 1:3 loop
        wrappedEulerDelta_rad[i] := wrapAngle(euler_rad[i] - pre(estimate.euler_rad[i]));
      end for;

      measuredSpeed := vectorNorm3(velocity_m_s);
      measuredFlightPathAngle :=
        asin(clip(velocity_m_s[3] / max(measuredSpeed, 1e-5), -1.0, 1.0));

      estimate.position_m :=
        lowPass(position_m, pre(estimate.position_m), filterSampleWeight);
      filteredEulerDelta_rad :=
        lowPass(wrappedEulerDelta_rad, zero3, filterSampleWeight);
      for i in 1:3 loop
        estimate.euler_rad[i] :=
          wrapAngle(pre(estimate.euler_rad[i]) + filteredEulerDelta_rad[i]);
      end for;
      estimate.velocity_m_s :=
        lowPass(velocity_m_s, pre(estimate.velocity_m_s), filterSampleWeight);
      estimate.eulerRate_rad_s :=
        lowPass(eulerRate_rad_s, pre(estimate.eulerRate_rad_s), filterSampleWeight);
      estimate.speed := lowPassScalar(measuredSpeed, pre(estimate.speed), filterSampleWeight);
      estimate.flightPathAngle :=
        lowPassScalar(measuredFlightPathAngle,
                      pre(estimate.flightPathAngle),
                      filterSampleWeight);
      estimate.acceleration_m_s2 :=
        (estimate.speed - pre(previousFilteredSpeed)) / dt;
      heldSamples := 0;
    elseif abs(position_m[1] - pre(previousPosition_m[1])) < 1e-12
        and abs(position_m[2] - pre(previousPosition_m[2])) < 1e-12
        and abs(position_m[3] - pre(previousPosition_m[3])) < 1e-12 then
      // A repeated pose carries no information; differencing it fakes zero
      // velocity, so hold the estimate until a fresh sample arrives.
      estimate.position_m := pre(estimate.position_m);
      estimate.euler_rad := pre(estimate.euler_rad);
      estimate.velocity_m_s := pre(estimate.velocity_m_s);
      estimate.eulerRate_rad_s := pre(estimate.eulerRate_rad_s);
      estimate.speed := pre(estimate.speed);
      estimate.flightPathAngle := pre(estimate.flightPathAngle);
      estimate.acceleration_m_s2 := pre(estimate.acceleration_m_s2);
      heldSamples := pre(heldSamples) + 1;
    elseif vectorNorm3({
        position_m[1] - pre(previousPosition_m[1]),
        position_m[2] - pre(previousPosition_m[2]),
        position_m[3] - pre(previousPosition_m[3])})
        / (dt * (1.0 + pre(heldSamples))) > jumpSpeedLimit
        and pre(heldSamples) * dt < jumpAcceptTime then
      // Implied speed is impossible for this airframe: reject the glitch
      // sample and hold, same as a repeated pose.
      estimate.position_m := pre(estimate.position_m);
      estimate.euler_rad := pre(estimate.euler_rad);
      estimate.velocity_m_s := pre(estimate.velocity_m_s);
      estimate.eulerRate_rad_s := pre(estimate.eulerRate_rad_s);
      estimate.speed := pre(estimate.speed);
      estimate.flightPathAngle := pre(estimate.flightPathAngle);
      estimate.acceleration_m_s2 := pre(estimate.acceleration_m_s2);
      heldSamples := pre(heldSamples) + 1;
    elseif vectorNorm3({
        position_m[1] - pre(previousPosition_m[1]),
        position_m[2] - pre(previousPosition_m[2]),
        position_m[3] - pre(previousPosition_m[3])})
        / (dt * (1.0 + pre(heldSamples))) > jumpSpeedLimit then
      // Offset persisted past jumpAcceptTime: tracking reacquired elsewhere.
      // Re-anchor pose, keep the last credible rates (a plane is never at rest).
      estimate.position_m := position_m;
      estimate.euler_rad := euler_rad;
      estimate.velocity_m_s := pre(estimate.velocity_m_s);
      estimate.eulerRate_rad_s := pre(estimate.eulerRate_rad_s);
      estimate.speed := pre(estimate.speed);
      estimate.flightPathAngle := pre(estimate.flightPathAngle);
      estimate.acceleration_m_s2 := pre(estimate.acceleration_m_s2);
      heldSamples := 0;
    else
      // Differencing spans held cycles, so a gap yields true velocity
      // instead of a divide-by-one-dt spike.
      sampleGap_s := dt * (1.0 + pre(heldSamples));
      for i in 1:3 loop
        rawVelocity_m_s[i] := (position_m[i] - pre(previousPosition_m[i])) / sampleGap_s;
        rawEulerRate_rad_s[i] :=
          wrapAngle(euler_rad[i] - pre(previousEuler_rad[i])) / sampleGap_s;
        wrappedEulerDelta_rad[i] := wrapAngle(euler_rad[i] - pre(estimate.euler_rad[i]));
      end for;
      heldSamples := 0;

      estimate.position_m :=
        lowPass(position_m, pre(estimate.position_m), filterSampleWeight);
      filteredEulerDelta_rad :=
        lowPass(wrappedEulerDelta_rad, zero3, filterSampleWeight);
      for i in 1:3 loop
        estimate.euler_rad[i] :=
          wrapAngle(pre(estimate.euler_rad[i]) + filteredEulerDelta_rad[i]);
      end for;
      estimate.velocity_m_s :=
        lowPass(rawVelocity_m_s, pre(estimate.velocity_m_s), velocitySampleWeight);
      estimate.eulerRate_rad_s :=
        lowPass(rawEulerRate_rad_s, pre(estimate.eulerRate_rad_s), filterSampleWeight);
      // Norm of the filtered vector: norm-of-raw is biased high by noise.
      estimate.speed := vectorNorm3(estimate.velocity_m_s);
      estimate.flightPathAngle :=
        asin(clip(estimate.velocity_m_s[3] / max(estimate.speed, 1e-5), -1.0, 1.0));
      rawAcceleration_m_s2 := (estimate.speed - pre(previousFilteredSpeed)) / sampleGap_s;
      estimate.acceleration_m_s2 :=
        lowPassScalar(rawAcceleration_m_s2,
                      pre(estimate.acceleration_m_s2),
                      accelSampleWeight);
    end if;

    previousPosition_m := position_m;
    previousEuler_rad := euler_rad;
    previousFilteredSpeed := estimate.speed;
  end when;
end StateEstimator;

block RouteGuidance
  parameter Real dt(unit = "s") = 0.02;
  parameter RouteParameters route = RouteParameters();

  input Boolean airborne;
  input FlightState estimate = FlightState();

  discrete output Integer currentWaypoint(min = 1, max = 6, start = 1);
  discrete output GuidanceSetpoints setpoints = GuidanceSetpoints();

protected
  discrete Integer activeWaypoint(min = 1, max = 6, start = 1);
  discrete Integer segmentEndIndex(min = 2, max = 7, start = 2);
  discrete Real segmentStart[3];
  discrete Real segmentEnd[3];
  discrete Real segmentVector[3];
  discrete Real horizontalSegmentVector[2];
  discrete Real horizontalSegmentLength;
  discrete Real segmentHeading;
  discrete Real segmentUnit[2];
  discrete Real crossTrackUnit[2];
  discrete Real positionFromSegmentStart[2];
  discrete Real alongTrackDistance;
  discrete Real remainingAlongTrackDistance;
  discrete Real pathProgress;
  discrete Real pathAltitude;
  discrete Real altitudeError;
  discrete Real crossTrackError;
  discrete Real steeringCorrection;

algorithm
  when sample(0.0, dt) then
    activeWaypoint := pre(currentWaypoint);
    segmentEndIndex := activeWaypoint + 1;
    segmentStart := waypointAt(route.waypoints, activeWaypoint);
    segmentEnd := waypointAt(route.waypoints, segmentEndIndex);

    segmentVector := segmentEnd - segmentStart;
    horizontalSegmentVector := horizontalPart(segmentVector);
    horizontalSegmentLength := max(vectorNorm2(horizontalSegmentVector), 1e-6);

    // Lateral path following is horizontal. Altitude is tracked against the
    // interpolated path altitude at the current along-track progress below.
    segmentHeading := atan2(horizontalSegmentVector[2], horizontalSegmentVector[1]);
    segmentUnit := horizontalSegmentVector / horizontalSegmentLength;
    crossTrackUnit := {-segmentUnit[2], segmentUnit[1]};
    positionFromSegmentStart :=
      horizontalDisplacement(estimate.position_m, segmentStart);
    alongTrackDistance := positionFromSegmentStart * segmentUnit;
    remainingAlongTrackDistance :=
      max(0.0,
          horizontalSegmentLength
          - clip(alongTrackDistance, 0.0, horizontalSegmentLength));
    pathProgress :=
      clip(alongTrackDistance / horizontalSegmentLength, 0.0, 1.0);
    pathAltitude :=
      segmentStart[3] + pathProgress * (segmentEnd[3] - segmentStart[3]);
    altitudeError := pathAltitude - estimate.position_m[3];
    crossTrackError := positionFromSegmentStart * crossTrackUnit;

    // Positive cross-track means the aircraft is left of the segment, so the
    // correction is negative to steer back toward the path.
    steeringCorrection :=
      atan2(-crossTrackError, max(route.crossTrackSteeringDistance, 1e-6));

    // Guidance remains live during the ground roll so the same TECS energy
    // controller used in flight can accelerate and throttle the takeoff. Keep
    // waypoint advancement inhibited until airborne.
    setpoints.speed := route.cruiseSpeed;
    setpoints.flightPathAngle :=
      clip(atan2(route.altitudeToFlightPathGain * altitudeError,
                 max(route.altitudeLookaheadDistance, 1e-6)),
           -route.flightPathAngleLimit,
           route.flightPathAngleLimit);
    setpoints.heading := wrapAngle(segmentHeading + steeringCorrection);
    setpoints.acceleration :=
      route.speedToAccelerationGain * (setpoints.speed - estimate.speed);

    // The endpoint guard is purely along track. Cross-track error must not
    // turn waypoint switching into a capture-radius test.
    if airborne and remainingAlongTrackDistance < route.waypointSwitchingDistance then
      currentWaypoint :=
        if activeWaypoint >= route.nSegments then 1 else activeWaypoint + 1;
    else
      currentWaypoint := activeWaypoint;
    end if;
  end when;
end RouteGuidance;

// TECS follows NASA CR-178285: thrust controls total energy rate while pitch
// redistributes energy between flight-path and speed. Lambregts' 2013 update
// keeps this normalization but adds saturation priority logic handled later.
block TECSController
  parameter Real dt(unit = "s") = 0.02;
  parameter VehicleParameters vehicle = VehicleParameters();
  parameter TecsParameters tecs = TecsParameters();

  input Boolean enabled;
  input GuidanceSetpoints setpoints = GuidanceSetpoints();
  input Real flightPathAngleEstimate(start = 0.0);
  input Real accelerationEstimate_m_s2(unit = "m/s2", start = 0.0);
  input Real rollAngle(unit = "rad", start = 0.0);

  discrete output Real boundedAcceleration(unit = "m/s2", start = 0.0);
  discrete output Real energyRateError(start = 0.0);
  discrete output Real energyRateIntegral(start = 0.0);
  discrete output Real unsaturatedThrustCommand(unit = "N", start = 0.0);
  discrete output Real thrustCommand(unit = "N", start = 0.0);
  discrete output Real energyDistributionError(start = 0.0);
  discrete output Real energyDistributionIntegral(start = 0.0);
  discrete output Real unsaturatedPitchCommand(unit = "rad", start = 0.0);
  discrete output Real pitchCommand(unit = "rad", start = 0.0);
  discrete output Real thrustPreview(unit = "N", start = 0.0)
    "integral-free thrust; what TECS would command while disabled";
  discrete output Real pitchPreview(unit = "rad", start = 0.0)
    "integral-free pitch; what TECS would command while disabled";

protected
  discrete Real gammaCommand(unit = "rad");
  discrete Real gammaEstimate(unit = "rad");
  discrete Real accelerationMin_m_s2(unit = "m/s2");
  discrete Real accelerationMax_m_s2(unit = "m/s2");
  discrete Real accelerationCommand_m_s2(unit = "m/s2");
  discrete Real accelerationCommandOverG;
  discrete Real accelerationEstimateOverG;
  discrete Real totalEnergyRateCommand;
  discrete Real totalEnergyRateEstimate;
  discrete Real energyDistributionCommand;
  discrete Real energyDistributionEstimate;
  discrete Real energyRateFeedforwardThrust(unit = "N");
  discrete Real loadFactorExcess;
  discrete Boolean thrustLimitedHigh;
  discrete Boolean thrustLimitedLow;
  discrete Boolean pitchLimitedHigh;
  discrete Boolean pitchLimitedLow;

algorithm
  when sample(0.0, dt) then
    // NASA CR-178285 uses normalized rate terms: gamma and Vdot / g.
    gammaCommand := setpoints.flightPathAngle;
    gammaEstimate := flightPathAngleEstimate;
    accelerationMin_m_s2 := -vehicle.drag / vehicle.mass;
    accelerationMax_m_s2 := (vehicle.thrustMax - vehicle.drag) / vehicle.mass;

    boundedAcceleration :=
      clip(setpoints.acceleration,
           accelerationMin_m_s2,
           accelerationMax_m_s2);

    accelerationCommand_m_s2 := boundedAcceleration;
    accelerationCommandOverG := accelerationCommand_m_s2 / vehicle.g;
    accelerationEstimateOverG := accelerationEstimate_m_s2 / vehicle.g;
    totalEnergyRateCommand := gammaCommand + accelerationCommandOverG;
    totalEnergyRateEstimate := gammaEstimate + accelerationEstimateOverG;
    energyDistributionCommand := gammaCommand - accelerationCommandOverG;
    energyDistributionEstimate := gammaEstimate - accelerationEstimateOverG;
    // Turn compensation: pay the banked-turn lift/drag bill as feedforward.
    loadFactorExcess := 1.0 / max(cos(rollAngle), 0.5) - 1.0;
    energyRateFeedforwardThrust :=
      vehicle.trimThrust
      + vehicle.weight
        * (totalEnergyRateCommand + tecs.turnThrustGain * loadFactorExcess);

    energyRateError := totalEnergyRateCommand - totalEnergyRateEstimate;
    energyDistributionError :=
      energyDistributionCommand - energyDistributionEstimate;
    thrustPreview :=
      clip(energyRateFeedforwardThrust
           + vehicle.weight * tecs.thrustKp * energyRateError,
           0.0, vehicle.thrustMax);
    pitchPreview :=
      clip(tecs.pitchKp * energyDistributionError
           + tecs.turnPitchGain * loadFactorExcess,
           -tecs.pitchCommandLimit,
           tecs.pitchCommandLimit);

    if enabled then
      unsaturatedThrustCommand :=
        energyRateFeedforwardThrust
        + vehicle.weight
          * (tecs.thrustKp * energyRateError
             + tecs.thrustKi * pre(energyRateIntegral));
      thrustCommand := clip(unsaturatedThrustCommand, 0.0, vehicle.thrustMax);
      thrustLimitedHigh := thrustCommand >= vehicle.thrustMax - 1e-9;
      thrustLimitedLow := thrustCommand <= 1e-9;

      // CUBS2 uses the original TECS saturation policy: clamp the command and
      // freeze only the integrator that would drive the effector farther into
      // saturation. It does not implement the Lambregts 2013 speed/path
      // priority allocator.
      if not ((thrustLimitedHigh and energyRateError > 0.0)
              or (thrustLimitedLow and energyRateError < 0.0)) then
        energyRateIntegral :=
          clip(pre(energyRateIntegral) + energyRateError * dt,
               -tecs.energyRateIntegralMax,
               tecs.energyRateIntegralMax);
      else
        energyRateIntegral := pre(energyRateIntegral);
      end if;

      unsaturatedPitchCommand :=
        tecs.pitchKp * energyDistributionError
        + tecs.pitchKi * pre(energyDistributionIntegral);
      pitchCommand :=
        clip(unsaturatedPitchCommand + tecs.turnPitchGain * loadFactorExcess,
             -tecs.pitchCommandLimit,
             tecs.pitchCommandLimit);
      pitchLimitedHigh := pitchCommand >= tecs.pitchCommandLimit - 1e-9;
      pitchLimitedLow := pitchCommand <= -tecs.pitchCommandLimit + 1e-9;

      if not ((pitchLimitedHigh and energyDistributionError > 0.0)
              or (pitchLimitedLow and energyDistributionError < 0.0)) then
        energyDistributionIntegral :=
          clip(pre(energyDistributionIntegral) + energyDistributionError * dt,
               -tecs.energyDistributionIntegralMax,
               tecs.energyDistributionIntegralMax);
      else
        energyDistributionIntegral := pre(energyDistributionIntegral);
      end if;
    else
      energyRateIntegral := 0.0;
      unsaturatedThrustCommand := vehicle.trimThrust;
      thrustCommand := vehicle.trimThrust;
      energyDistributionIntegral := 0.0;
      unsaturatedPitchCommand := 0.0;
      pitchCommand := 0.0;
      thrustLimitedHigh := false;
      thrustLimitedLow := false;
      pitchLimitedHigh := false;
      pitchLimitedLow := false;
    end if;
  end when;
end TECSController;

block AttitudeController
  parameter Real dt(unit = "s") = 0.02;
  parameter VehicleParameters vehicle = VehicleParameters();
  parameter AttitudeParameters params = AttitudeParameters();
  parameter PidParameters headingPid =
    PidParameters(dt = dt, useInputDerivative = true, kp = 0.5, ki = 0.0,
                  kd = 0.5, integralMax = 0.0,
                  commandMin = -params.rollLimit,
                  commandMax = params.rollLimit);
  // Closed pitch loop: the plant's stick-to-pitch response is not trusted,
  // so the open-loop gain rides along as feedforward and the PID trims it.
  parameter PidParameters pitchPid =
    PidParameters(dt = dt, kp = 2.3, ki = 0.0, kd = 0.0, integralMax = 0.0,
                  commandMin = -1.0, commandMax = 1.0);

  PidController headingController(params = headingPid);
  PidController pitchController(params = pitchPid);

  input Boolean airborne;
  input Boolean engaged;
  input Boolean climbout;
  input GuidanceSetpoints setpoints = GuidanceSetpoints();
  input FlightState estimate = FlightState();
  input Real tecsPitchCommand(unit = "rad", start = 0.0);
  input Real tecsThrustCommand(unit = "N", start = 0.0);

  discrete output Real aileron(start = 0.0);
  discrete output Real elevator(start = 0.0);
  discrete output Real throttle(start = 0.7);
  discrete output Real rudder(start = 0.0);
  discrete output Real rollCommand(start = 0.0);
  discrete output Real rollCommandPreview(start = 0.0)
    "integral-free bank; what guidance would fly while disengaged";
  discrete output Real courseError(start = 0.0);

protected
  discrete Real rollCommandState(start = 0.0);
  discrete Real climboutPitchTarget(start = 0.0);
  discrete Real course;
  discrete Real headingErrorRate;
  discrete Real unlimitedRollCommand;

algorithm
  when sample(0.0, dt) then
    if not airborne then
      throttle := clip(tecsThrustCommand / vehicle.thrustMax, 0.0, 1.0);
      elevator := params.takeoffElevator;
      aileron := 0.0;
      rudder := 0.0;
      rollCommandState := pre(rollCommandState);
      rollCommand := rollCommandState;
      rollCommandPreview := 0.0;
      courseError := 0.0;
      headingErrorRate := 0.0;
      unlimitedRollCommand := 0.0;

      headingController.enabled := false;
      headingController.error := 0.0;
      headingController.derivativeInput := 0.0;
      headingController.feedforward := 0.0;
      pitchController.enabled := false;
      pitchController.error := 0.0;
      pitchController.derivativeInput := 0.0;
      pitchController.feedforward := 0.0;
    elseif climbout then
      // Keep the fixed-pitch climb and gentle bank limit, but use TECS for
      // total-energy/throttle control from the ground roll onward.
      throttle := clip(tecsThrustCommand / vehicle.thrustMax, 0.0, 1.0);
      rudder := 0.0;

      course := atan2(estimate.velocity_m_s[2], estimate.velocity_m_s[1]);
      courseError := -wrapAngle(setpoints.heading - course);
      if abs(courseError) < params.courseDeadband then
        courseError := 0.0;
      end if;
      headingErrorRate := wrapAngle(courseError - pre(courseError)) / dt;

      headingController.enabled := engaged;
      headingController.error := courseError;
      headingController.derivativeInput := headingErrorRate;
      headingController.feedforward := 0.0;

      unlimitedRollCommand :=
        clip(headingController.command,
             -params.climboutRollLimit, params.climboutRollLimit);
      rollCommandState :=
        rateLimit(unlimitedRollCommand,
                  pre(rollCommandState),
                  params.rollRateLimit * dt);
      rollCommandState :=
        clip(rollCommandState, -params.climboutRollLimit, params.climboutRollLimit);
      rollCommand := rollCommandState;
      rollCommandPreview :=
        clip(headingController.preview,
             -params.climboutRollLimit, params.climboutRollLimit);
      aileron := clip(params.rollCommandToAileronGain * rollCommand, -1.0, 1.0);

      climboutPitchTarget :=
        clip(params.climboutPitch
             + params.climboutSpeedGain * (estimate.speed - setpoints.speed),
             params.climboutPitch, params.climboutMaxPitch);
      pitchController.enabled := engaged;
      pitchController.error := climboutPitchTarget - estimate.euler_rad[2];
      pitchController.derivativeInput := 0.0;
      pitchController.feedforward :=
        params.trimElevator + params.pitchCommandToElevatorGain * climboutPitchTarget;
      elevator := clip(pitchController.command,
                       params.climboutMinElevator, params.climboutMaxElevator);
    else
      throttle := clip(tecsThrustCommand / vehicle.thrustMax, 0.0, 1.0);

      pitchController.enabled := engaged;
      pitchController.error := tecsPitchCommand - estimate.euler_rad[2];
      pitchController.derivativeInput := 0.0;
      pitchController.feedforward :=
        params.trimElevator + params.pitchCommandToElevatorGain * tecsPitchCommand;
      elevator := pitchController.command;

      course := atan2(estimate.velocity_m_s[2], estimate.velocity_m_s[1]);
      courseError := -wrapAngle(setpoints.heading - course);
      if courseError * courseError < params.courseDeadband * params.courseDeadband then
        courseError := 0.0;
      end if;
      headingErrorRate := wrapAngle(courseError - pre(courseError)) / dt;

      headingController.enabled := engaged;
      headingController.error := courseError;
      headingController.derivativeInput := headingErrorRate;
      headingController.feedforward := 0.0;

      unlimitedRollCommand := headingController.command;
      rollCommandState :=
        rateLimit(unlimitedRollCommand,
                  pre(rollCommandState),
                  params.rollRateLimit * dt);
      rollCommandState := clip(rollCommandState, -params.rollLimit, params.rollLimit);
      rollCommand := rollCommandState;
      rollCommandPreview :=
        clip(headingController.preview, -params.rollLimit, params.rollLimit);
      aileron := clip(params.rollCommandToAileronGain * rollCommand, -1.0, 1.0);
      rudder := 0.0;
    end if;
  end when;
end AttitudeController;

model FixedWingOuterLoop
  parameter Real dt(unit = "s") = 0.02
    "50 Hz outer loop (lockstep: 2 plant steps of 0.01 per packet)";
  parameter VehicleParameters vehicle = VehicleParameters();
  parameter RouteParameters route = RouteParameters();
  parameter TecsParameters tecsParams = TecsParameters();
  parameter AttitudeParameters attitudeParams = AttitudeParameters();
  parameter Real filterCutoffHz(unit = "Hz") = 10.0;

  StateEstimator estimator(dt = dt, filterCutoffHz = filterCutoffHz);
  RouteGuidance guidance(dt = dt, route = route);
  TECSController tecs(dt = dt, vehicle = vehicle, tecs = tecsParams);
  AttitudeController attitude(dt = dt, vehicle = vehicle, params = attitudeParams);

  input Real position_m[3](each unit = "m") "current sample [x, y, z] [m]";
  input Real euler_rad[3](each unit = "rad") "current sample [roll, pitch, yaw] [rad]";
  input Real velocity_m_s[3](each unit = "m/s") "current velocity sample [x, y, z] [m/s]";
  input Real eulerRate_rad_s[3](each unit = "rad/s") "current body-rate sample [roll, pitch, yaw] [rad/s]";
  input Real engaged(min = 0.0, max = 1.0)
    "1 when auto mode drives the servos; integrators stay reset otherwise";

  discrete output Real aileron(start = 0.0) "aileron stick [-1, 1]";
  discrete output Real elevator(start = 0.0) "elevator stick [-1, 1]";
  discrete output Real throttle(start = 0.7) "throttle stick [0, 1]";
  discrete output Real rudder(start = 0.0) "rudder stick [-1, 1]";
  discrete output Real stabilizer(start = 2000.0) "onboard stabilizer PWM [us]";
  discrete output Boolean airborne(start = false);
  discrete output Integer currentWaypoint(min = 1, max = 6, start = 1);
  discrete output Real desiredSpeed(start = 0.0);
  discrete output Real desiredFlightPathAngle(start = 0.0);
  discrete output Real desiredHeading(start = 0.0);
  discrete output Real desiredAcceleration(start = 0.0);
  discrete output Real rollCommand(start = 0.0);
  discrete output Real rollCommandPreview(start = 0.0);
  discrete output Real pitchCommandPreview(start = 0.0);
  discrete output Real courseError(start = 0.0);
  discrete output Real positionEstimate_m[3](each start = 0.0);
  discrete output Real eulerEstimate_rad[3](each start = 0.0);
  discrete output Real velocityEstimate_m_s[3](each start = 0.0);
  discrete output Real speedEstimate(start = 0.0);
  discrete output Real flightPathAngleEstimate(start = 0.0);
  discrete output Real accelerationEstimate_m_s2(unit = "m/s2", start = 0.0);
  discrete output Real eulerRateEstimate_rad_s[3](each start = 0.0);

protected
  discrete Integer groundedSamples(start = 0);
  discrete Boolean climboutDone(start = false);
  discrete Boolean climbout(start = false);
  discrete Integer handoffSamples(start = 0);

algorithm
  when sample(0.0, dt) then
    estimator.position_m := position_m;
    estimator.euler_rad := euler_rad;
    estimator.velocity_m_s := velocity_m_s;
    estimator.eulerRate_rad_s := eulerRate_rad_s;

    // Latched above takeoffAltitude; a sustained stay near the ground
    // re-arms the takeoff sequence so a crash or reset is self-recovering.
    if estimator.estimate.position_m[3] < 0.75 * attitudeParams.takeoffAltitude then
      groundedSamples := pre(groundedSamples) + 1;
    else
      groundedSamples := 0;
    end if;
    airborne :=
      (pre(airborne)
       and groundedSamples * dt < attitudeParams.groundedResetTime)
      or (estimator.estimate.position_m[3] > attitudeParams.takeoffAltitude);
    climboutDone :=
      airborne
      and (pre(climboutDone)
           or (estimator.estimate.position_m[3] > attitudeParams.climboutAltitude
               and estimator.estimate.speed < guidance.setpoints.speed + 0.5)
           or estimator.estimate.position_m[3] > 2.5);
    climbout := airborne and not climboutDone;
    if climboutDone then
      handoffSamples := pre(handoffSamples) + 1;
    else
      handoffSamples := 0;
    end if;
    guidance.airborne := airborne;
    guidance.estimate := estimator.estimate;

    tecs.enabled := engaged > 0.5;
    tecs.setpoints := guidance.setpoints;
    tecs.flightPathAngleEstimate := estimator.estimate.flightPathAngle;
    tecs.accelerationEstimate_m_s2 := estimator.estimate.acceleration_m_s2;
    tecs.rollAngle := estimator.estimate.euler_rad[1];

    attitude.airborne := airborne;
    attitude.engaged := engaged > 0.5;
    attitude.climbout := climbout;
    attitude.setpoints := guidance.setpoints;
    attitude.estimate := estimator.estimate;
    attitude.tecsPitchCommand := tecs.pitchCommand;
    attitude.tecsThrustCommand := tecs.thrustCommand;

    aileron := attitude.aileron;
    elevator := attitude.elevator;
    throttle := attitude.throttle;
    rudder := attitude.rudder;
    // Soft handoff: freshly enabled TECS can believe a transient over-speed
    // and slam nose-down with throttle cut; floor both while it settles.
    if climboutDone and handoffSamples * dt < attitudeParams.handoffGuardTime then
      elevator := max(elevator, attitudeParams.climboutMinElevator);
      throttle := max(throttle, attitudeParams.handoffMinThrottle);
    end if;
    // Pose frozen/rejected too long while airborne: neutral sticks so the
    // onboard SAFE stabilizer levels the wings, throttle back.
    if airborne and estimator.heldSamples * dt > attitudeParams.staleFailsafeTime then
      aileron := 0.0;
      elevator := 0.0;
      rudder := 0.0;
      throttle := attitudeParams.staleFailsafeThrottle;
    end if;
    stabilizer := attitudeParams.stabilizerCommand;
    rollCommand := attitude.rollCommand;
    rollCommandPreview := attitude.rollCommandPreview;
    pitchCommandPreview := tecs.pitchPreview;
    courseError := attitude.courseError;

    currentWaypoint := guidance.currentWaypoint;
    desiredSpeed := guidance.setpoints.speed;
    desiredFlightPathAngle := guidance.setpoints.flightPathAngle;
    desiredHeading := guidance.setpoints.heading;
    desiredAcceleration := guidance.setpoints.acceleration;
    positionEstimate_m := estimator.estimate.position_m;
    eulerEstimate_rad := estimator.estimate.euler_rad;
    velocityEstimate_m_s := estimator.estimate.velocity_m_s;
    speedEstimate := estimator.estimate.speed;
    flightPathAngleEstimate := estimator.estimate.flightPathAngle;
    accelerationEstimate_m_s2 := estimator.estimate.acceleration_m_s2;
    eulerRateEstimate_rad_s := estimator.estimate.eulerRate_rad_s;
  end when;
end FixedWingOuterLoop;
