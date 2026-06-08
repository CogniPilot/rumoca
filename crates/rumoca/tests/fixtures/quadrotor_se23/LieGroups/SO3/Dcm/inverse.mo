within LieGroups.SO3.Dcm;
function inverse "SO(3) DCM inverse: transpose"
  input Real R[3,3];
  output Real R_inv[3,3];
algorithm
  for i in 1:3 loop
    for j in 1:3 loop
      R_inv[i,j] := R[j,i];
    end for;
  end for;
end inverse;
