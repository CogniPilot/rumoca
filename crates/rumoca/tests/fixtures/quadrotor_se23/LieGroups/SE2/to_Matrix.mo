within LieGroups.SE2;
function to_Matrix "Convert SE(2) element to 3x3 homogeneous matrix"
  input Real X[3] "{px, py, theta}";
  output Real M[3,3] "3x3 matrix {{R, p}, {0, 1}}";
protected
  Real c, s;
algorithm
  c := cos(X[3]);
  s := sin(X[3]);
  M[1,1] := c;   M[1,2] := -s;  M[1,3] := X[1];
  M[2,1] := s;   M[2,2] := c;   M[2,3] := X[2];
  M[3,1] := 0;   M[3,2] := 0;   M[3,3] := 1;
end to_Matrix;
