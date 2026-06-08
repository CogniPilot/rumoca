within LieGroups.SE2;
function adjoint "Adjoint Ad_X for SE(2), translation-first ordering"
  input Real X[3] "{px, py, theta}";
  output Real Ad[3,3] "3x3 adjoint matrix";
protected
  Real c, s;
algorithm
  c := cos(X[3]);
  s := sin(X[3]);
  // Ad_X = {{R(theta), [py, -px]^T}, {0, 0, 1}}
  Ad[1,1] := c;   Ad[1,2] := -s;  Ad[1,3] := X[2];
  Ad[2,1] := s;   Ad[2,2] := c;   Ad[2,3] := -X[1];
  Ad[3,1] := 0;   Ad[3,2] := 0;   Ad[3,3] := 1;
end adjoint;
