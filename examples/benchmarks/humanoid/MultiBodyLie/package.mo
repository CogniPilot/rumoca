package MultiBodyLie "MSL MultiBody rewritten on articulated-body connectors (layout and names follow Modelica.Mechanics.MultiBody)"
  // Conventions (see Interfaces.Frame). Spatial motion {w; v} and force {t; f} are resolved in the frame and taken about
  // its origin; spatial accelerations include gravity as a fictitious upward acceleration of the root. In each
  // connection set exactly one component owns the frame's kinematics (World, a joint's child-side frame, a rigid
  // translation's far frame, a free-flight root Body) and receives the flows; every other component defines its flow
  // contributions (f, t, pA, IA) and dv = 0.

  model World "Inertial root with uniform gravity (no animation)"
    parameter Boolean enableAnimation = true "Ignored: no animation";
    parameter Types.GravityTypes gravityType = Types.GravityTypes.UniformGravity "NoGravity or UniformGravity (PointGravity unsupported)";
    parameter Real g = 9.80665 "Gravity constant [m/s2] (MSL default Modelica.Constants.g_n)";
    parameter Real n[3] = {0, -1, 0} "Direction of gravity (MSL default -y)";
    Interfaces.Frame_b frame_b;
  equation
    Connections.root(frame_b.R);
    frame_b.r_0 = zeros(3);
    frame_b.R.T = identity(3);
    frame_b.R.w = zeros(3);
    frame_b.v = zeros(3);
    frame_b.a = cat(1, zeros(3), (if gravityType == Types.GravityTypes.NoGravity then 0 else -g) * n / sqrt(n * n));
    frame_b.dI = zeros(21);
  end World;

  package Frames
    record Orientation "MSL-compatible orientation: T maps world to frame coordinates; w is the angular velocity in frame coordinates"
      Real T[3, 3];
      Real w[3];
      encapsulated function equalityConstraint "Residue of two orientations that should be equal (overconstrained connector)"
        import MultiBodyLie.Frames.Orientation;
        input Orientation R1;
        input Orientation R2;
        output Real residue[3];
      protected
        Real Tr[3, 3];
      algorithm
        Tr := R1.T * transpose(R2.T);
        residue := {0.5 * (Tr[2, 3] - Tr[3, 2]), 0.5 * (Tr[3, 1] - Tr[1, 3]), 0.5 * (Tr[1, 2] - Tr[2, 1])};
      end equalityConstraint;
    end Orientation;

    function resolve1 "Vector resolved in the frame of R -> world"
      input Orientation R;
      input Real v2[3];
      output Real v1[3];
    algorithm
      v1 := transpose(R.T) * v2;
    end resolve1;

    function resolve2 "Vector in world coordinates -> resolved in the frame of R"
      input Orientation R;
      input Real v1[3];
      output Real v2[3];
    algorithm
      v2 := R.T * v1;
    end resolve2;
  end Frames;

  package Types
    type GravityTypes = enumeration(NoGravity "No gravity field", UniformGravity "Uniform gravity field", PointGravity "Point gravity field (unsupported)");
    type ResolveInFrameB = enumeration(world "Resolve in world frame", frame_b "Resolve in frame_b", frame_resolve "Resolve in frame_resolve (unsupported)");
  end Types;

  package Interfaces
    connector Frame "MSL Frame variables plus the articulated-body quantities, balanced pairwise"
      Real r_0[3] "Origin, world";
      Frames.Orientation R;
      flow Real f[3] "Cut force on the component, frame";
      flow Real t[3] "Cut torque on the component about the origin, frame";
      Real v[3] "Velocity of the origin, frame";
      flow Real dv[3] "Balances v; zero for every non-owner";
      Real a[6] "Spatial acceleration incl. fictitious gravity, frame";
      flow Real pA[6] "Bias wrench contribution";
      Real dI[21] "Balances IA; no physics, zero";
      flow Real IA[21] "Articulated inertia contribution, symmetric 6x6 packed (Math.packSym)";
    end Frame;
    connector Frame_a = Frame;
    connector Frame_b = Frame;
  end Interfaces;

  package Math
    function skew
      input Real a[3];
      output Real S[3, 3];
    algorithm
      S := {{0, -a[3], a[2]}, {a[3], 0, -a[1]}, {-a[2], a[1], 0}};
    end skew;

    function axisRotation "Parent <- child rotation by angle about unit axis n (Rodrigues)"
      input Real n[3];
      input Real angle;
      output Real R[3, 3];
    algorithm
      R := cos(angle) * identity(3) + sin(angle) * skew(n) + (1 - cos(angle)) * {{n[i] * n[j] for j in 1:3} for i in 1:3};
    end axisRotation;

    function crm "Spatial motion cross product matrix: crm(V) * m = V x m"
      input Real V[6];
      output Real X[6, 6];
    algorithm
      X := zeros(6, 6);
      X[1:3, 1:3] := skew(V[1:3]);
      X[4:6, 1:3] := skew(V[4:6]);
      X[4:6, 4:6] := skew(V[1:3]);
    end crm;

    function motionTransform "Spatial transform parent -> child: child <- parent rotation E, child origin at r (parent coordinates)"
      input Real E[3, 3];
      input Real r[3];
      output Real X[6, 6];
    algorithm
      X := zeros(6, 6);
      X[1:3, 1:3] := E;
      X[4:6, 1:3] := -E * skew(r);
      X[4:6, 4:6] := E;
    end motionTransform;

    function translateInertia "X' S X for the pure translation X = motionTransform(identity(3), r), on packed symmetric 6x6"
      // With S = [A, B; B', C] and X = [I, 0; -[r]x, I]: C' = C, B' = B + [r]x C, A' = A + [r]x B' - B [r]x - [r]x C [r]x.
      // About 110 multiplications instead of two dense 6x6 products.
      input Real p[21];
      input Real r[3];
      output Real q[21];
    protected
      Real A[3, 3] = {{p[1], p[2], p[3]}, {p[2], p[7], p[8]}, {p[3], p[8], p[12]}};
      Real B[3, 3] = {{p[4], p[5], p[6]}, {p[9], p[10], p[11]}, {p[13], p[14], p[15]}};
      Real C[3, 3] = {{p[16], p[17], p[18]}, {p[17], p[19], p[20]}, {p[18], p[20], p[21]}};
      Real rx[3, 3] = skew(r);
      Real rxC[3, 3] = rx * C;
      Real Bn[3, 3] = B + rxC;
      Real An[3, 3] = A + rx * transpose(B) - B * rx - rxC * rx;
    algorithm
      q := {An[1, 1], An[1, 2], An[1, 3], Bn[1, 1], Bn[1, 2], Bn[1, 3], An[2, 2], An[2, 3], Bn[2, 1], Bn[2, 2], Bn[2, 3],
            An[3, 3], Bn[3, 1], Bn[3, 2], Bn[3, 3], p[16], p[17], p[18], p[19], p[20], p[21]};
    end translateInertia;

    function rotateInertia "X' S X for the pure rotation X = motionTransform(R, zeros(3)) = diag(R, R), on packed symmetric 6x6"
      // Blockwise: A' = R' A R, B' = R' B R, C' = R' C R (four 3x3 congruences instead of two dense 6x6 products).
      input Real p[21];
      input Real R[3, 3];
      output Real q[21];
    protected
      Real A[3, 3] = transpose(R) * {{p[1], p[2], p[3]}, {p[2], p[7], p[8]}, {p[3], p[8], p[12]}} * R;
      Real B[3, 3] = transpose(R) * {{p[4], p[5], p[6]}, {p[9], p[10], p[11]}, {p[13], p[14], p[15]}} * R;
      Real C[3, 3] = transpose(R) * {{p[16], p[17], p[18]}, {p[17], p[19], p[20]}, {p[18], p[20], p[21]}} * R;
    algorithm
      q := {A[1, 1], A[1, 2], A[1, 3], B[1, 1], B[1, 2], B[1, 3], A[2, 2], A[2, 3], B[2, 1], B[2, 2], B[2, 3],
            A[3, 3], B[3, 1], B[3, 2], B[3, 3], C[1, 1], C[1, 2], C[1, 3], C[2, 2], C[2, 3], C[3, 3]};
    end rotateInertia;

    function spatialInertia "Spatial inertia about the frame origin of mass m at c with inertia I about the centre of mass"
      input Real m;
      input Real c[3];
      input Real I[3, 3];
      output Real Is[6, 6];
    protected
      Real cx[3, 3] = skew(c);
    algorithm
      Is := zeros(6, 6);
      Is[1:3, 1:3] := I + m * cx * transpose(cx);
      Is[1:3, 4:6] := m * cx;
      Is[4:6, 1:3] := m * transpose(cx);
      Is[4:6, 4:6] := m * identity(3);
    end spatialInertia;


    function packSym "Upper triangle of a symmetric 6x6, row-major (21 entries)"
      input Real M[6, 6];
      output Real p[21];
    algorithm
      p := {M[1, 1], M[1, 2], M[1, 3], M[1, 4], M[1, 5], M[1, 6], M[2, 2], M[2, 3], M[2, 4], M[2, 5], M[2, 6],
            M[3, 3], M[3, 4], M[3, 5], M[3, 6], M[4, 4], M[4, 5], M[4, 6], M[5, 5], M[5, 6], M[6, 6]};
    end packSym;

    function unpackSym "Symmetric 6x6 from its packed upper triangle"
      input Real p[21];
      output Real M[6, 6];
    algorithm
      M := {{p[1], p[2], p[3], p[4], p[5], p[6]},
            {p[2], p[7], p[8], p[9], p[10], p[11]},
            {p[3], p[8], p[12], p[13], p[14], p[15]},
            {p[4], p[9], p[13], p[16], p[17], p[18]},
            {p[5], p[10], p[14], p[17], p[19], p[20]},
            {p[6], p[11], p[15], p[18], p[20], p[21]}};
    end unpackSym;

    function quaternionToT "World -> frame rotation matrix of a quaternion {w, x, y, z} (frame -> world rotation)"
      input Real Q[4];
      output Real T[3, 3];
    protected
      Real q[4] = Q / sqrt(Q * Q);
    algorithm
      T := transpose({{1 - 2 * (q[3] ^ 2 + q[4] ^ 2), 2 * (q[2] * q[3] - q[1] * q[4]), 2 * (q[2] * q[4] + q[1] * q[3])},
                      {2 * (q[2] * q[3] + q[1] * q[4]), 1 - 2 * (q[2] ^ 2 + q[4] ^ 2), 2 * (q[3] * q[4] - q[1] * q[2])},
                      {2 * (q[2] * q[4] - q[1] * q[3]), 2 * (q[3] * q[4] + q[1] * q[2]), 1 - 2 * (q[2] ^ 2 + q[3] ^ 2)}});
    end quaternionToT;

    function solveSPD "Solve A x = b for a symmetric positive definite A (Cholesky); explicit for the compiler"
      input Real A[:, size(A, 1)];
      input Real b[size(A, 1)];
      output Real x[size(A, 1)];
    protected
      Integer n = size(A, 1);
      Real L[size(A, 1), size(A, 1)];
      Real s;
    algorithm
      L := zeros(n, n);
      for j in 1:n loop
        s := A[j, j];
        for k in 1:j - 1 loop
          s := s - L[j, k] ^ 2;
        end for;
        L[j, j] := sqrt(s);
        for i in j + 1:n loop
          s := A[i, j];
          for k in 1:j - 1 loop
            s := s - L[i, k] * L[j, k];
          end for;
          L[i, j] := s / L[j, j];
        end for;
      end for;
      x := b;
      for i in 1:n loop
        for k in 1:i - 1 loop
          x[i] := x[i] - L[i, k] * x[k];
        end for;
        x[i] := x[i] / L[i, i];
      end for;
      for i in n:-1:1 loop
        for k in i + 1:n loop
          x[i] := x[i] - L[k, i] * x[k];
        end for;
        x[i] := x[i] / L[i, i];
      end for;
    end solveSPD;
  end Math;

  package Parts
    model Body "Rigid body at frame_a; a free-flight root when nothing roots its frame (MSL names)"
      outer MultiBodyLie.World world;
      Interfaces.Frame_a frame_a;
      parameter Boolean animation = true "Ignored: no animation";
      parameter Real r_CM[3] = zeros(3);
      parameter Real m = 1;
      parameter Real I_11 = 0.001, I_22 = 0.001, I_33 = 0.001, I_21 = 0, I_31 = 0, I_32 = 0;
      parameter Boolean angles_fixed = false "Root only: fix the start orientation (angles_start = 0 supported)";
      parameter Boolean w_0_fixed = false "Root only: fix the start angular velocity";
      parameter Real w_0_start[3] = zeros(3);
      final parameter Real Is[6, 6] = Math.spatialInertia(m, r_CM, {{I_11, I_21, I_31}, {I_21, I_22, I_32}, {I_31, I_32, I_33}});
      Real r_0[3](start = zeros(3)) "Origin of frame_a, world";
      Real v_0[3](start = zeros(3)) "Velocity of frame_a's origin, world";
      Real a_0[3] "Acceleration of frame_a's origin, world";
      Real w_a[3](start = w_0_start, each fixed = w_0_fixed) "Angular velocity, frame_a";
      Real Q[4](start = {1, 0, 0, 0}) "Root orientation, frame -> world quaternion";
      // Gravity in a binding: the same expression in the equation section read the World class default instead of the
      // inner modifier under rumoca b3afe4cb (reported, unminimised).
      Real gravity_0[3] = (if world.gravityType == Types.GravityTypes.NoGravity then 0 else world.g) * {world.n[1], world.n[2], world.n[3]} / sqrt(world.n[1] ^ 2 + world.n[2] ^ 2 + world.n[3] ^ 2) "Gravity, world";
    protected
      Real V[6] = cat(1, frame_a.R.w, frame_a.v);
      Real h[6] = Is * V "Spatial momentum";
      Real bias[6] = cat(1, cross(V[1:3], h[1:3]) + cross(V[4:6], h[4:6]), cross(V[1:3], h[4:6])) "v x* (I v) = -crm(V)' (I V)";
      Real aTrue[6] "Spatial acceleration without the fictitious gravity, frame_a";
    initial equation
      // As MSL: angles_fixed fixes frame_a's orientation whichever body the tool picks as the root
      // (angles_start = 0 supported: the start orientation is the world's).
      if angles_fixed then
        if Connections.isRoot(frame_a.R) then
          Q[2:4] = zeros(3);
        else
          zeros(3) = {0.5 * (frame_a.R.T[2, 3] - frame_a.R.T[3, 2]), 0.5 * (frame_a.R.T[3, 1] - frame_a.R.T[1, 3]), 0.5 * (frame_a.R.T[1, 2] - frame_a.R.T[2, 1])};
        end if;
      end if;
    equation
      Connections.potentialRoot(frame_a.R);
      if Connections.isRoot(frame_a.R) then
        frame_a.r_0 = r_0;
        frame_a.R.T = Math.quaternionToT(Q);
        frame_a.R.w = w_a;
        frame_a.v = frame_a.R.T * v_0;
        frame_a.dI = zeros(21);
        frame_a.a = -Math.solveSPD(Is - Math.unpackSym(frame_a.IA), bias - frame_a.pA) "explicit: the whole tree's articulated inertia at the root";
        der(r_0) = v_0;
        der(Q) = 0.5 * {-Q[2] * w_a[1] - Q[3] * w_a[2] - Q[4] * w_a[3], Q[1] * w_a[1] + Q[3] * w_a[3] - Q[4] * w_a[2], Q[1] * w_a[2] - Q[2] * w_a[3] + Q[4] * w_a[1], Q[1] * w_a[3] + Q[2] * w_a[2] - Q[3] * w_a[1]};
        der(w_a) = aTrue[1:3];
        der(v_0) = a_0;
      else
        frame_a.IA = Math.packSym(Is);
        frame_a.pA = bias;
        frame_a.dv = zeros(3);
        cat(1, frame_a.t, frame_a.f) = Is * frame_a.a + bias;
        r_0 = frame_a.r_0;
        v_0 = transpose(frame_a.R.T) * frame_a.v;
        w_a = frame_a.R.w;
        Q = {1, 0, 0, 0};
      end if;
      aTrue = frame_a.a + cat(1, zeros(3), frame_a.R.T * gravity_0);
      a_0 = transpose(frame_a.R.T) * (aTrue[4:6] + cross(frame_a.R.w, frame_a.v));
    end Body;

    model FixedTranslation "Rigid offset r from frame_a to frame_b (frame_a coordinates); either direction"
      Interfaces.Frame_a frame_a;
      Interfaces.Frame_b frame_b;
      parameter Boolean animation = true "Ignored: no animation";
      parameter Real r[3] = zeros(3);
    equation
      // Pure translation: X a = {a1, a2 - r x a1}, X' p = {p1 + r x p2, p2}, X' S X by Math.translateInertia.
      Connections.branch(frame_a.R, frame_b.R);
      if Connections.rooted(frame_a.R) then
        frame_b.r_0 = frame_a.r_0 + transpose(frame_a.R.T) * r;
        frame_b.R.T = frame_a.R.T;
        frame_b.R.w = frame_a.R.w;
        frame_b.v = frame_a.v + cross(frame_a.R.w, r);
        frame_b.a = cat(1, frame_a.a[1:3], frame_a.a[4:6] - cross(r, frame_a.a[1:3]));
        frame_b.dI = zeros(21);
        frame_a.IA = -Math.translateInertia(frame_b.IA, r);
        frame_a.pA = -cat(1, frame_b.pA[1:3] + cross(r, frame_b.pA[4:6]), frame_b.pA[4:6]);
        cat(1, frame_a.t, frame_a.f) = Math.unpackSym(frame_a.IA) * frame_a.a + frame_a.pA;
        frame_a.dv = zeros(3);
      else
        frame_a.r_0 = frame_b.r_0 - transpose(frame_b.R.T) * r;
        frame_a.R.T = frame_b.R.T;
        frame_a.R.w = frame_b.R.w;
        frame_a.v = frame_b.v - cross(frame_b.R.w, r);
        frame_a.a = cat(1, frame_b.a[1:3], frame_b.a[4:6] + cross(r, frame_b.a[1:3]));
        frame_a.dI = zeros(21);
        frame_b.IA = -Math.translateInertia(frame_a.IA, -r);
        frame_b.pA = -cat(1, frame_a.pA[1:3] - cross(r, frame_a.pA[4:6]), frame_a.pA[4:6]);
        cat(1, frame_b.t, frame_b.f) = Math.unpackSym(frame_b.IA) * frame_b.a + frame_b.pA;
        frame_b.dv = zeros(3);
      end if;
    end FixedTranslation;

    model BodyShape "Rigid body with frame_a and frame_b (r from a to b) and mass at r_CM (MSL names)"
      Interfaces.Frame_a frame_a;
      Interfaces.Frame_b frame_b;
      parameter Boolean animation = true "Ignored: no animation";
      parameter Real r[3] = zeros(3);
      parameter Real r_CM[3] = zeros(3);
      parameter Real m = 1;
      parameter Real I_11 = 0.001, I_22 = 0.001, I_33 = 0.001, I_21 = 0, I_31 = 0, I_32 = 0;
      Real r_0[3] "Origin of frame_a, world";
      Real v_0[3] "Velocity of frame_a's origin, world";
      Real a_0[3];
      FixedTranslation frameTranslation(r = r);
      Body body(r_CM = r_CM, m = m, I_11 = I_11, I_22 = I_22, I_33 = I_33, I_21 = I_21, I_31 = I_31, I_32 = I_32);
    equation
      r_0 = frame_a.r_0;
      v_0 = body.v_0;
      a_0 = body.a_0;
      connect(frame_a, frameTranslation.frame_a);
      connect(frame_b, frameTranslation.frame_b);
      connect(frame_a, body.frame_a);
    end BodyShape;
  end Parts;

  package Joints
    model Revolute "Revolute joint about n, optionally driven through its axis flange; either direction (MSL names)"
      Modelica.Mechanics.Rotational.Interfaces.Flange_a axis "Always present; unconnected means tau = 0 (useAxisFlange kept for MSL compatibility)";
      Modelica.Mechanics.Rotational.Interfaces.Flange_b support "Joint housing on frame_a: angle 0; its reaction is internal to the joint torque";
      Interfaces.Frame_a frame_a;
      Interfaces.Frame_b frame_b;
      parameter Boolean useAxisFlange = false;
      parameter Boolean animation = true "Ignored: no animation";
      parameter Real n[3] = {0, 0, 1};
      parameter StateSelect stateSelect = StateSelect.prefer;
      final parameter Real e[3] = n / sqrt(n * n);
      Real phi(start = 0, stateSelect = stateSelect) "Rotation of frame_b relative to frame_a about e";
      Real w(start = 0, stateSelect = stateSelect);
      Real a "= der(w)";
      Real tau "Driving torque";
    protected
      // Pure rotation: X = diag(E, E) (frame_b <- frame_a), applied blockwise; X' S X by Math.rotateInertia.
      Real E[3, 3] = transpose(Math.axisRotation(e, phi)) "frame_b <- frame_a";
      Real S[6] "Motion subspace in child coordinates";
      Real IAc[6, 6];
      Real pAc[6];
      Real Vc[6];
      Real ap[6];
      Real c[6];
      Real U[6];
      Real D;
      Real u;
      Real Ia[6, 6];
      Real pa[6];
    equation
      Connections.branch(frame_a.R, frame_b.R);
      axis.phi = phi;
      support.phi = 0;
      tau = axis.tau;
      w = der(phi);
      a = der(w);
      if Connections.rooted(frame_a.R) then
        S = cat(1, e, zeros(3));
        frame_b.r_0 = frame_a.r_0;
        frame_b.R.T = E * frame_a.R.T;
        frame_b.R.w = E * frame_a.R.w + e * w;
        frame_b.v = E * frame_a.v;
        Vc = cat(1, frame_b.R.w, frame_b.v);
        IAc = -Math.unpackSym(frame_b.IA);
        pAc = -frame_b.pA;
        ap = cat(1, E * frame_a.a[1:3], E * frame_a.a[4:6]);
        frame_b.a = ap + c + S * a;
        frame_b.dI = zeros(21);
        frame_a.IA = Math.rotateInertia(Math.packSym(Ia), E);
        frame_a.pA = cat(1, transpose(E) * pa[1:3], transpose(E) * pa[4:6]);
        cat(1, frame_a.t, frame_a.f) = Math.unpackSym(frame_a.IA) * frame_a.a + frame_a.pA;
        frame_a.dv = zeros(3);
      else
        S = cat(1, -e, zeros(3));
        frame_a.r_0 = frame_b.r_0;
        frame_a.R.T = transpose(E) * frame_b.R.T;
        frame_a.R.w = transpose(E) * frame_b.R.w - e * w;
        frame_a.v = transpose(E) * frame_b.v;
        Vc = cat(1, frame_a.R.w, frame_a.v);
        IAc = -Math.unpackSym(frame_a.IA);
        pAc = -frame_a.pA;
        ap = cat(1, transpose(E) * frame_b.a[1:3], transpose(E) * frame_b.a[4:6]);
        frame_a.a = ap + c + S * a;
        frame_a.dI = zeros(21);
        frame_b.IA = Math.rotateInertia(Math.packSym(Ia), transpose(E));
        frame_b.pA = cat(1, E * pa[1:3], E * pa[4:6]);
        cat(1, frame_b.t, frame_b.f) = Math.unpackSym(frame_b.IA) * frame_b.a + frame_b.pA;
        frame_b.dv = zeros(3);
      end if;
      c = cat(1, cross(Vc[1:3], S[1:3] * w), cross(Vc[4:6], S[1:3] * w)) "crm(Vc) (S w) with S = {+-e, 0}";
      U = IAc * S;
      D = S * U;
      u = tau - S * pAc;
      Ia = IAc - {{U[i] * U[j] for j in 1:6} for i in 1:6} / D;
      pa = pAc + Ia * c + U * (u / D);
      a = (u - U * (ap + c)) / D;
    end Revolute;
  end Joints;

  package Forces
    model WorldForce "External force acting at frame_b (MSL names; resolveInFrame world or frame_b)"
      Modelica.Blocks.Interfaces.RealInput force[3];
      Interfaces.Frame_b frame_b;
      parameter Types.ResolveInFrameB resolveInFrame = Types.ResolveInFrameB.world;
      parameter Boolean animation = true "Ignored: no animation";
    equation
      frame_b.f = -(if resolveInFrame == Types.ResolveInFrameB.world then frame_b.R.T * force else force);
      frame_b.t = zeros(3);
      frame_b.pA = cat(1, frame_b.t, frame_b.f);
      frame_b.IA = zeros(21);
      frame_b.dv = zeros(3);
    end WorldForce;

    model WorldForceAndTorque "External force and torque acting at frame_b (MSL names; resolveInFrame world or frame_b)"
      Modelica.Blocks.Interfaces.RealInput force[3];
      Modelica.Blocks.Interfaces.RealInput torque[3];
      Interfaces.Frame_b frame_b;
      parameter Types.ResolveInFrameB resolveInFrame = Types.ResolveInFrameB.world;
      parameter Boolean animation = true "Ignored: no animation";
    equation
      frame_b.f = -(if resolveInFrame == Types.ResolveInFrameB.world then frame_b.R.T * force else force);
      frame_b.t = -(if resolveInFrame == Types.ResolveInFrameB.world then frame_b.R.T * torque else torque);
      frame_b.pA = cat(1, frame_b.t, frame_b.f);
      frame_b.IA = zeros(21);
      frame_b.dv = zeros(3);
    end WorldForceAndTorque;
  end Forces;
end MultiBodyLie;
