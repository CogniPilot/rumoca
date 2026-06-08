within LieGroups.SO3.Mrp;
function to_DCM "Convert MRP to rotation matrix"
  input Real r[3] "MRP parameters";
  output Real R[3,3] "Rotation matrix";
protected
  Real n_sq;
  Real den_sq;
  Real S[3,3] "Skew [r]x";
  Real S2[3,3] "[r]x^2";
algorithm
  n_sq := r[1]^2 + r[2]^2 + r[3]^2;
  den_sq := (1.0 + n_sq) * (1.0 + n_sq);

  S := {{0, -r[3], r[2]},
        {r[3], 0, -r[1]},
        {-r[2], r[1], 0}};

  S2 := {{-(r[2]^2 + r[3]^2), r[1]*r[2], r[1]*r[3]},
         {r[1]*r[2], -(r[1]^2 + r[3]^2), r[2]*r[3]},
         {r[1]*r[3], r[2]*r[3], -(r[1]^2 + r[2]^2)}};

  // R = I + (8*S^2 - 4*(1-n_sq)*S) / (1+n_sq)^2
  for i in 1:3 loop
    for j in 1:3 loop
      R[i,j] := (if i == j then 1.0 else 0.0)
                + (8.0*S2[i,j] - 4.0*(1.0 - n_sq)*S[i,j]) / den_sq;
    end for;
  end for;
end to_DCM;
