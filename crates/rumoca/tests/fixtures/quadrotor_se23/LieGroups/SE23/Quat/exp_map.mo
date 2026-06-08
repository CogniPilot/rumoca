within LieGroups.SE23.Quat;
function exp_map "Exponential map: se_2(3) -> SE_2(3) via 5x5 matrix series"
  input Real xi[9] "Lie algebra {vb, ab, omega}";
  output Real X[10] "Group element {p, v, q}";
protected
  Real omega[3];
  Real theta_sq;
  Real C1 "(1-cos theta)/theta^2";
  Real C2 "(theta-sin theta)/theta^3";
  Real theta;
  constant Real eps = 1e-8;
  // 5x5 matrix approach: build algebra matrix, compute I + M*(I + C1*M + C2*M^2)
  Real M[5,5] "Lie algebra matrix";
  Real M2[5,5] "M*M";
  Real inner_mat[5,5] "I + C1*M + C2*M^2";
  Real result[5,5] "I + M*inner_mat";
  Real R_mat[3,3];
  Real q[4];
algorithm
  omega := xi[7:9];
  theta_sq := omega[1]^2 + omega[2]^2 + omega[3]^2;

  if theta_sq < eps then
    C1 := 0.5 - theta_sq / 24.0;
    C2 := 1.0/6.0 - theta_sq / 120.0;
  else
    theta := sqrt(theta_sq);
    C1 := (1.0 - cos(theta)) / theta_sq;
    C2 := (theta - sin(theta)) / (theta_sq * theta);
  end if;

  // Build 5x5 algebra matrix:
  // [omega^  a_b  v_b]
  // [0       0    0  ]
  // [0       0    0  ]
  for i in 1:5 loop
    for j in 1:5 loop
      M[i,j] := 0;
    end for;
  end for;
  // Rotation block (top-left 3x3)
  M[1,2] := -omega[3]; M[1,3] := omega[2];
  M[2,1] := omega[3];  M[2,3] := -omega[1];
  M[3,1] := -omega[2]; M[3,2] := omega[1];
  // Acceleration column (col 4)
  M[1,4] := xi[4]; M[2,4] := xi[5]; M[3,4] := xi[6];
  // Velocity column (col 5)
  M[1,5] := xi[1]; M[2,5] := xi[2]; M[3,5] := xi[3];

  // M^2
  for i in 1:5 loop
    for j in 1:5 loop
      M2[i,j] := 0;
      for k in 1:5 loop
        M2[i,j] := M2[i,j] + M[i,k]*M[k,j];
      end for;
    end for;
  end for;

  // inner_mat = I + C1*M + C2*M^2
  for i in 1:5 loop
    for j in 1:5 loop
      inner_mat[i,j] := (if i == j then 1.0 else 0.0) + C1*M[i,j] + C2*M2[i,j];
    end for;
  end for;

  // result = I + M * inner_mat
  for i in 1:5 loop
    for j in 1:5 loop
      result[i,j] := if i == j then 1.0 else 0.0;
      for k in 1:5 loop
        result[i,j] := result[i,j] + M[i,k]*inner_mat[k,j];
      end for;
    end for;
  end for;

  // Extract: position = col 5 (rows 1-3), velocity = col 4 (rows 1-3)
  // rotation = top-left 3x3 -> quaternion
  for i in 1:3 loop
    for j in 1:3 loop
      R_mat[i,j] := result[i,j];
    end for;
  end for;
  q := LieGroups.SO3.Quat.from_DCM(R_mat);

  // Note: cyecca from_Matrix swaps columns: p = col5, v = col4
  X[1] := result[1,5]; X[2] := result[2,5]; X[3] := result[3,5];
  X[4] := result[1,4]; X[5] := result[2,4]; X[6] := result[3,4];
  X[7] := q[1]; X[8] := q[2]; X[9] := q[3]; X[10] := q[4];
end exp_map;
