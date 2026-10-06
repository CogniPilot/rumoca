// Bounded cyclic Jacobi eigensolver for a symmetric 6x6 registration information
// matrix. The caller validates finite symmetric input. Columns are orthonormal
// eigenvectors; eigenvalues are sorted ascending. No artificial diagonal floor
// is introduced: planar registration must retain its unobservable directions.
function SymmetricEigen6
  input Real matrix[6,6];
  output Real values[6];
  output Real vectors[6,6];
protected
  Real work[6,6];
  Real norm;
  Real tau;
  Real tangent;
  Real cosine;
  Real sine;
  Real first;
  Real second;
  Real offDiagonal;
  Real temporary;
  Real active;
  Real swap;
algorithm
  work := matrix;
  vectors := identity(6);
  norm := 0.0;
  for i in 1:6 loop
    for j in 1:6 loop
      norm := max(norm,abs(work[i,j]));
    end for;
  end for;
  for sweep in 1:32 loop
    for p in 1:5 loop
      for q in 1:6 loop
        offDiagonal := work[p,q];
          active := if q > p and abs(offDiagonal) > 1e-15*norm then 1.0 else 0.0;
          tau := (work[q,q]-work[p,p])/(if active > 0.0 then 2.0*offDiagonal else 1.0);
          tangent := if active <= 0.0 then 0.0 else if tau >= 0.0 then 1.0/(tau+sqrt(1.0+tau*tau))
            else -1.0/(-tau+sqrt(1.0+tau*tau));
          cosine := 1.0/sqrt(1.0+tangent*tangent);
          sine := tangent*cosine;
          work[p,p] := work[p,p]-tangent*offDiagonal;
          work[q,q] := work[q,q]+tangent*offDiagonal;
          work[p,q] := if active > 0.0 then 0.0 else offDiagonal;
          work[q,p] := work[p,q];
          for k in 1:6 loop
              first := work[k,p];
              second := work[k,q];
              work[k,p] := if k <> p and k <> q then cosine*first-sine*second else first;
              work[p,k] := if k <> p and k <> q then work[k,p] else work[p,k];
              work[k,q] := if k <> p and k <> q then sine*first+cosine*second else second;
              work[q,k] := if k <> p and k <> q then work[k,q] else work[q,k];
            first := vectors[k,p];
            second := vectors[k,q];
            vectors[k,p] := cosine*first-sine*second;
            vectors[k,q] := sine*first+cosine*second;
          end for;
      end for;
    end for;
  end for;
  for i in 1:6 loop
    values[i] := work[i,i];
  end for;
  for i in 1:5 loop
    for j in 1:6 loop
        swap := if j > i and values[j] < values[i] then 1.0 else 0.0;
        temporary := values[i];
        values[i] := if swap > 0.0 then values[j] else values[i];
        values[j] := if swap > 0.0 then temporary else values[j];
        for k in 1:6 loop
          temporary := vectors[k,i];
          vectors[k,i] := if swap > 0.0 then vectors[k,j] else vectors[k,i];
          vectors[k,j] := if swap > 0.0 then temporary else vectors[k,j];
        end for;
    end for;
  end for;
end SymmetricEigen6;

// Standalone mathematical gate; the full registration worker is not wired yet.
model RegistrationEigen6
  input Real information[6,6] = identity(6);
  output Real eigenvalues[6];
  output Real eigenvectors[6,6];
equation
  (eigenvalues,eigenvectors) = SymmetricEigen6(information);
end RegistrationEigen6;
