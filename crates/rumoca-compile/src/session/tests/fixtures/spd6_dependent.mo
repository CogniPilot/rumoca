// Shared Cholesky factorization of a 6×6 geometric innovation covariance.
// Solve sixteen right-hand sides: cross-covariance transpose plus innovation.
// Inputs must be finite; the host checks shape/range before copying to WASM.
// valid=0 rejects nonsymmetric or nonpositive covariance and returns zeros.
// Rejected factors use a unit diagonal to keep subsequent arithmetic bounded.
model SPD6Solve
  parameter Real pivot_floor = 1e-12;
  parameter Real symmetry_absolute = 1e-12;
  parameter Real symmetry_relative = 1e-8;
  input Real A[6,6] = identity(6);
  input Real B[6,16] = fill(0.0,6,16);
  output Real X[6,16];
  output Real valid;
protected
  Real L[6,6];
  Real pivot[6];
  Real Z[6,16];
  Real solution[6,16];
  Real symmetryChecks[6,6];
  Real symmetry_errors;
equation
  for i in 1:6 loop
    for j in 1:6 loop
      symmetryChecks[i,j] = if noEvent(abs(A[i,j]-A[j,i]) <=
        symmetry_absolute+symmetry_relative*abs(A[j,i])) then 0.0 else 1.0;
    end for;
  end for;
  symmetry_errors = sum(symmetryChecks[i,j] for i in 1:6, j in 1:6);
  pivot[1] = A[1,1];
  for i in 2:6 loop
    pivot[i] = A[i,i]-sum(L[i,k]*L[i,k] for k in 1:i-1);
    L[i,1] = A[i,1]/L[1,1];
    for j in 2:i-1 loop
      L[i,j] = (A[i,j]-sum(L[i,k]*L[j,k] for k in 1:j-1))/L[j,j];
    end for;
  end for;
  for i in 1:6 loop
    L[i,i] = sqrt(if noEvent(pivot[i] > pivot_floor) then pivot[i] else 1.0);
    for j in i+1:6 loop
      L[i,j] = 0.0;
    end for;
  end for;
  valid = if noEvent(symmetry_errors < 0.5 and
    pivot[1] > pivot_floor and pivot[2] > pivot_floor and pivot[3] > pivot_floor and
    pivot[4] > pivot_floor and pivot[5] > pivot_floor and pivot[6] > pivot_floor) then 1.0 else 0.0;
  for column in 1:16 loop
    Z[1,column] = B[1,column]/L[1,1];
    for i in 2:6 loop
      Z[i,column] = (B[i,column]-sum(L[i,k]*Z[k,column] for k in 1:i-1))/L[i,i];
    end for;
    solution[6,column] = Z[6,column]/L[6,6];
    for i in 1:5 loop
      solution[i,column] = (Z[i,column]-sum(L[k,i]*solution[k,column] for k in i+1:6))/L[i,i];
    end for;
    for i in 1:6 loop
      X[i,column] = if noEvent(valid > 0.5) then solution[i,column] else 0.0;
    end for;
  end for;
end SPD6Solve;
