within LieGroups.SE2;
function product "SE(2) group product: (t1,R1) * (t2,R2) = (R1*t2+t1, R1*R2)"
  input Real X1[3] "{px1, py1, theta1}";
  input Real X2[3] "{px2, py2, theta2}";
  output Real X[3] "{px, py, theta}";
protected
  Real c1, s1;
algorithm
  c1 := cos(X1[3]);
  s1 := sin(X1[3]);
  // p = R(theta1) * p2 + p1
  X[1] := c1*X2[1] - s1*X2[2] + X1[1];
  X[2] := s1*X2[1] + c1*X2[2] + X1[2];
  // theta = theta1 + theta2
  X[3] := X1[3] + X2[3];
end product;
