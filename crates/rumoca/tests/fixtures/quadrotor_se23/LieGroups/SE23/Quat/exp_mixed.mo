within LieGroups.SE23.Quat;
function exp_mixed "Mixed exponential for SE_2(3) INS propagation"
  input Real X0[10] "Initial state {p0, v0, q0}";
  input Real l[9] "Left (body-frame) algebra element {vb, ab, omega_l}";
  input Real r[9] "Right (world-frame) algebra element {vb_r, ab_r, omega_r}";
  input Real B[2,2] "Coupling matrix (typically {{0,1},{0,0}})";
  output Real X1[10] "Updated state {p1, v1, q1}";
protected
  Real omega_l[3], omega_r[3];
  Real theta_sq;
  Real C1, C2, C3;
  Real theta;
  constant Real eps = 1e-8;

  // Omega matrix and its square
  Real Om[3,3], Om2[3,3];

  // A matrices (3x2): [a_b | v_b] for left and right
  Real Al[3,2], Ar[3,2];

  // N matrices (3x2): calculated for left and right
  Real Nl[3,2], Nr[3,2];

  // Intermediate products
  Real OmAl[3,2], Om2Al[3,2];
  Real OmAr[3,2], Om2Ar[3,2];
  Real AlB[3,2], ArBn[3,2];
  Real I2[2,2];
  Real IpB[2,2], ImB[2,2];

  // Rotation quaternions
  Real q_l[4], q_r[4], q_r0[4], q1[4];
  Real R_r0[3,3], R_r[3,3];

  // P matrices (3x2): [v | p]
  Real P0[3,2], P1[3,2];
  Real term1[3,2], term2[3,2], term3[3,2];
algorithm
  omega_l := l[7:9];
  omega_r := r[7:9];

  theta_sq := omega_l[1]^2 + omega_l[2]^2 + omega_l[3]^2;

  if theta_sq < eps then
    C1 := 0.5 - theta_sq / 24.0;
    C2 := 1.0/6.0 - theta_sq / 120.0;
    C3 := 1.0/24.0 - theta_sq / 720.0;
  else
    theta := sqrt(theta_sq);
    C1 := (1.0 - cos(theta)) / theta_sq;
    C2 := (theta - sin(theta)) / (theta_sq * theta);
    // C3 = (theta^2/2 + cos(theta) - 1) / theta^4
    C3 := (theta_sq/2.0 + cos(theta) - 1.0) / (theta_sq * theta_sq);
  end if;

  // Build Omega (skew of omega_l) and Omega^2
  Om := LieGroups.SO3.Quat.wedge(omega_l);
  for i in 1:3 loop
    for j in 1:3 loop
      Om2[i,j] := Om[i,1]*Om[1,j] + Om[i,2]*Om[2,j] + Om[i,3]*Om[3,j];
    end for;
  end for;

  // A_l = [a_b | v_b] (columns: acceleration, velocity from left algebra)
  Al := {{l[4], l[1]}, {l[5], l[2]}, {l[6], l[3]}};
  Ar := {{r[4], r[1]}, {r[5], r[2]}, {r[6], r[3]}};

  I2 := {{1, 0}, {0, 1}};

  // Omega*A and Omega^2*A for left
  for i in 1:3 loop
    for k in 1:2 loop
      OmAl[i,k] := Om[i,1]*Al[1,k] + Om[i,2]*Al[2,k] + Om[i,3]*Al[3,k];
      Om2Al[i,k] := Om2[i,1]*Al[1,k] + Om2[i,2]*Al[2,k] + Om2[i,3]*Al[3,k];
    end for;
  end for;

  // A*B for left
  for i in 1:3 loop
    for k in 1:2 loop
      AlB[i,k] := Al[i,1]*B[1,k] + Al[i,2]*B[2,k];
    end for;
  end for;

  // N_l = A + A*B/2 + Om*A*(C1*I + C2*B) + Om^2*A*(C2*I + C3*B)
  for i in 1:3 loop
    for k in 1:2 loop
      Nl[i,k] := Al[i,k] + AlB[i,k]/2.0;
      // Om*A*(C1*I + C2*B)
      for j in 1:2 loop
        Nl[i,k] := Nl[i,k] + OmAl[i,j]*(C1*(if j==k then 1.0 else 0.0) + C2*B[j,k]);
      end for;
      // Om^2*A*(C2*I + C3*B)
      for j in 1:2 loop
        Nl[i,k] := Nl[i,k] + Om2Al[i,j]*(C2*(if j==k then 1.0 else 0.0) + C3*B[j,k]);
      end for;
    end for;
  end for;

  // N_r: same formula but with r algebra and -B
  // Recompute with omega_r for the rotation part (but cyecca uses omega_l for both N computations)
  // Actually cyecca uses l's omega for Nl and r's omega for Nr - but r's omega is typically 0 or small
  // For simplicity and matching cyecca: Nr uses the same series but with -B and r's A
  // The Omega in Nr should use r's omega
  // For the standard INS case, r has omega_r = 0, so Om_r = 0 and Nr = Ar + Ar*(-B)/2
  // Let me implement the general case with r's omega:

  // For Nr, we need omega_r's series coefficients
  theta_sq := omega_r[1]^2 + omega_r[2]^2 + omega_r[3]^2;
  if theta_sq < eps then
    C1 := 0.5 - theta_sq / 24.0;
    C2 := 1.0/6.0 - theta_sq / 120.0;
    C3 := 1.0/24.0 - theta_sq / 720.0;
  else
    theta := sqrt(theta_sq);
    C1 := (1.0 - cos(theta)) / theta_sq;
    C2 := (theta - sin(theta)) / (theta_sq * theta);
    C3 := (theta_sq/2.0 + cos(theta) - 1.0) / (theta_sq * theta_sq);
  end if;

  Om := LieGroups.SO3.Quat.wedge(omega_r);
  for i in 1:3 loop
    for j in 1:3 loop
      Om2[i,j] := Om[i,1]*Om[1,j] + Om[i,2]*Om[2,j] + Om[i,3]*Om[3,j];
    end for;
  end for;

  for i in 1:3 loop
    for k in 1:2 loop
      OmAr[i,k] := Om[i,1]*Ar[1,k] + Om[i,2]*Ar[2,k] + Om[i,3]*Ar[3,k];
      Om2Ar[i,k] := Om2[i,1]*Ar[1,k] + Om2[i,2]*Ar[2,k] + Om2[i,3]*Ar[3,k];
      ArBn[i,k] := -(Ar[i,1]*B[1,k] + Ar[i,2]*B[2,k]);
    end for;
  end for;

  for i in 1:3 loop
    for k in 1:2 loop
      Nr[i,k] := Ar[i,k] + ArBn[i,k]/2.0;
      for j in 1:2 loop
        Nr[i,k] := Nr[i,k] + OmAr[i,j]*(C1*(if j==k then 1.0 else 0.0) - C2*B[j,k]);
      end for;
      for j in 1:2 loop
        Nr[i,k] := Nr[i,k] + Om2Ar[i,j]*(C2*(if j==k then 1.0 else 0.0) - C3*B[j,k]);
      end for;
    end for;
  end for;

  // Rotation updates
  q_l := LieGroups.SO3.Quat.exp_map(omega_l);
  q_r := LieGroups.SO3.Quat.exp_map(omega_r);
  q_r0 := LieGroups.SO3.Quat.product(q_r, X0[7:10]);
  q1 := LieGroups.SO3.Quat.product(q_r0, q_l);

  R_r0 := LieGroups.SO3.Quat.to_DCM(q_r0);
  R_r := LieGroups.SO3.Quat.to_DCM(q_r);

  // P0 = [v0 | p0]
  P0 := {{X0[4], X0[1]}, {X0[5], X0[2]}, {X0[6], X0[3]}};

  // I + B
  IpB := {{1 + B[1,1], B[1,2]}, {B[2,1], 1 + B[2,2]}};

  // P1 = R_r0 * Nl + (R_r * P0 + Nr) * (I + B)
  // term1 = R_r0 * Nl
  for i in 1:3 loop
    for k in 1:2 loop
      term1[i,k] := R_r0[i,1]*Nl[1,k] + R_r0[i,2]*Nl[2,k] + R_r0[i,3]*Nl[3,k];
    end for;
  end for;

  // term2 = R_r * P0 + Nr
  for i in 1:3 loop
    for k in 1:2 loop
      term2[i,k] := R_r[i,1]*P0[1,k] + R_r[i,2]*P0[2,k] + R_r[i,3]*P0[3,k] + Nr[i,k];
    end for;
  end for;

  // term3 = term2 * (I + B)
  for i in 1:3 loop
    for k in 1:2 loop
      term3[i,k] := term2[i,1]*IpB[1,k] + term2[i,2]*IpB[2,k];
    end for;
  end for;

  P1 := {{term1[i,k] + term3[i,k] for k in 1:2} for i in 1:3};

  // Extract: p = P1 col 2, v = P1 col 1 (cyecca swaps: vertcat(P1[:,1], P1[:,0], R1))
  X1[1] := P1[1,2]; X1[2] := P1[2,2]; X1[3] := P1[3,2];
  X1[4] := P1[1,1]; X1[5] := P1[2,1]; X1[6] := P1[3,1];
  X1[7] := q1[1]; X1[8] := q1[2]; X1[9] := q1[3]; X1[10] := q1[4];
end exp_mixed;
