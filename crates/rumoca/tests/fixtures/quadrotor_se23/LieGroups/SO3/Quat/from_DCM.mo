within LieGroups.SO3.Quat;
function from_DCM "Convert 3x3 rotation matrix to unit quaternion (Shepperd's method)"
  input Real R[3,3] "Rotation matrix";
  output Real q[4] "Unit quaternion {w,x,y,z}";
protected
  Real tr;
  Real b1, b2, b3, b4;
  constant Real eps = 1e-10;
algorithm
  tr := R[1,1] + R[2,2] + R[3,3];

  if tr > 0 then
    // Case 1: trace > 0 (w is largest)
    b1 := 0.5 * sqrt(max(1.0 + tr, eps));
    q[1] := b1;
    q[2] := (R[3,2] - R[2,3]) / max(4.0*b1, eps);
    q[3] := (R[1,3] - R[3,1]) / max(4.0*b1, eps);
    q[4] := (R[2,1] - R[1,2]) / max(4.0*b1, eps);
  elseif R[1,1] > R[2,2] and R[1,1] > R[3,3] then
    // Case 2: R[1,1] is largest diagonal
    b2 := 0.5 * sqrt(max(1.0 + R[1,1] - R[2,2] - R[3,3], eps));
    q[1] := (R[3,2] - R[2,3]) / max(4.0*b2, eps);
    q[2] := b2;
    q[3] := (R[1,2] + R[2,1]) / max(4.0*b2, eps);
    q[4] := (R[1,3] + R[3,1]) / max(4.0*b2, eps);
  elseif R[2,2] > R[3,3] then
    // Case 3: R[2,2] is largest diagonal
    b3 := 0.5 * sqrt(max(1.0 - R[1,1] + R[2,2] - R[3,3], eps));
    q[1] := (R[1,3] - R[3,1]) / max(4.0*b3, eps);
    q[2] := (R[1,2] + R[2,1]) / max(4.0*b3, eps);
    q[3] := b3;
    q[4] := (R[2,3] + R[3,2]) / max(4.0*b3, eps);
  else
    // Case 4: R[3,3] is largest diagonal
    b4 := 0.5 * sqrt(max(1.0 - R[1,1] - R[2,2] + R[3,3], eps));
    q[1] := (R[2,1] - R[1,2]) / max(4.0*b4, eps);
    q[2] := (R[1,3] + R[3,1]) / max(4.0*b4, eps);
    q[3] := (R[2,3] + R[3,2]) / max(4.0*b4, eps);
    q[4] := b4;
  end if;

  // Ensure positive w convention. If-EXPRESSION (not a no-else if-statement) so
  // AD/both-branch compilers evaluate the condition correctly.
  q := if q[1] < 0 then -q else q;
end from_DCM;
