within LieGroups.SE2;
function inverse "SE(2) inverse: (t,R)^{-1} = (-R^T*t, -theta)"
  input Real X[3] "{px, py, theta}";
  output Real X_inv[3] "{px_inv, py_inv, theta_inv}";
protected
  Real c, s;
algorithm
  c := cos(X[3]);
  s := sin(X[3]);
  // p_inv = -R(-theta) * p = -R^T * p
  X_inv[1] := -(c*X[1] + s*X[2]);
  X_inv[2] := -(-s*X[1] + c*X[2]);
  X_inv[3] := -X[3];
end inverse;
