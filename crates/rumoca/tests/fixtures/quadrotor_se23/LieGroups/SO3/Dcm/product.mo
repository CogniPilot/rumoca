within LieGroups.SO3.Dcm;
function product "SO(3) DCM product: R1 * R2"
  input Real R1[3,3];
  input Real R2[3,3];
  output Real R[3,3];
algorithm
  for i in 1:3 loop
    for j in 1:3 loop
      R[i,j] := R1[i,1]*R2[1,j] + R1[i,2]*R2[2,j] + R1[i,3]*R2[3,j];
    end for;
  end for;
end product;
