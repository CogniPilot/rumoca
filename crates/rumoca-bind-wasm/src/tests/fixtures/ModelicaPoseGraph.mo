// Bounded pose-graph numerical component; no loop detection or truth inputs.
// Body-to-world R, world-additive p and RIGHT-local attitude increments.
// Residual [Ri'*(pj-pi)-translation, Log(measuredRotation'*Ri'*Rj)].
// This product residual is not the coupled SE(3) logarithm (no V^-1 translation).
// Small numerical helpers are editable Modelica, not host math.
function PGTranspose
  input Real A[3,3]; output Real B[3,3];
algorithm
  B[1,1] := A[1,1];
  B[2,1] := A[1,2];
  B[3,1] := A[1,3];
  B[1,2] := A[2,1];
  B[2,2] := A[2,2];
  B[3,2] := A[2,3];
  B[1,3] := A[3,1];
  B[2,3] := A[3,2];
  B[3,3] := A[3,3];
end PGTranspose;

function PGMultiply
  input Real A[3,3]; input Real B[3,3]; output Real C[3,3];
algorithm
  C[1,1] := A[1,1]*B[1,1]+A[1,2]*B[2,1]+A[1,3]*B[3,1];
  C[1,2] := A[1,1]*B[1,2]+A[1,2]*B[2,2]+A[1,3]*B[3,2];
  C[1,3] := A[1,1]*B[1,3]+A[1,2]*B[2,3]+A[1,3]*B[3,3];
  C[2,1] := A[2,1]*B[1,1]+A[2,2]*B[2,1]+A[2,3]*B[3,1];
  C[2,2] := A[2,1]*B[1,2]+A[2,2]*B[2,2]+A[2,3]*B[3,2];
  C[2,3] := A[2,1]*B[1,3]+A[2,2]*B[2,3]+A[2,3]*B[3,3];
  C[3,1] := A[3,1]*B[1,1]+A[3,2]*B[2,1]+A[3,3]*B[3,1];
  C[3,2] := A[3,1]*B[1,2]+A[3,2]*B[2,2]+A[3,3]*B[3,2];
  C[3,3] := A[3,1]*B[1,3]+A[3,2]*B[2,3]+A[3,3]*B[3,3];
end PGMultiply;

function PGMatVec
  input Real A[3,3]; input Real x[3]; output Real y[3];
algorithm
  y[1] := A[1,1]*x[1]+A[1,2]*x[2]+A[1,3]*x[3];
  y[2] := A[2,1]*x[1]+A[2,2]*x[2]+A[2,3]*x[3];
  y[3] := A[3,1]*x[1]+A[3,2]*x[2]+A[3,3]*x[3];
end PGMatVec;

function PGSkew
  input Real x[3]; output Real S[3,3];
algorithm
  S := zeros(3,3);
  S[1,2] := -x[3]; S[1,3] := x[2]; S[2,1] := x[3];
  S[2,3] := -x[1]; S[3,1] := -x[2]; S[3,2] := x[1];
end PGSkew;

function PGProperRotation
  input Real R[3,3]; output Boolean valid;
protected Real value; Real determinant;
algorithm
  valid := true; value := 0.0;
  for i in 1:3 loop
    for j in 1:3 loop
      valid := valid and abs(R[i,j]) <= 1.000001;
      value := 0.0;
      for k in 1:3 loop value := value+R[k,i]*R[k,j]; end for;
      valid := valid and abs(value-(if i == j then 1.0 else 0.0)) <= 1e-7;
    end for;
  end for;
  determinant := R[1,1]*(R[2,2]*R[3,3]-R[2,3]*R[3,2])
    -R[1,2]*(R[2,1]*R[3,3]-R[2,3]*R[3,1])+R[1,3]*(R[2,1]*R[3,2]-R[2,2]*R[3,1]);
  valid := valid and abs(determinant-1.0) <= 1e-7;
end PGProperRotation;

function PGExp
  input Real angle[3]; output Real R[3,3];
protected Real square; Real theta; Real a; Real b; Real S[3,3];
algorithm
  square := angle[1]*angle[1]+angle[2]*angle[2]+angle[3]*angle[3]; theta := sqrt(max(square,0.0));
  a := if square < 1e-8 then 1.0-square/6.0+square*square/120.0 else sin(theta)/max(theta,1e-12);
  b := if square < 1e-8 then 0.5-square/24.0+square*square/720.0 else (1.0-cos(theta))/max(square,1e-12);
  S := PGSkew(angle); R := identity(3)+a*S+b*PGMultiply(S,S);
end PGExp;

function PGLog
  input Real R[3,3]; output Real angle[3]; output Boolean valid;
protected Real cosine; Real theta; Real square; Real scale;
algorithm
  cosine := min(1.0,max(-1.0,(R[1,1]+R[2,2]+R[3,3]-1.0)/2.0));
  theta := acos(cosine); square := theta*theta;
  // The chart is deliberately refused near its nonunique pi branch.
  valid := theta >= 0.0 and theta < 3.140592653589793;
  scale := if square < 1e-8 then 0.5+square/12.0+7.0*square*square/720.0
    else theta/(2.0*max(sin(theta),1e-12));
  angle := if valid then scale*{R[3,2]-R[2,3],R[1,3]-R[3,1],R[2,1]-R[1,2]} else zeros(3);
end PGLog;

function PGEdge
  input Real pi[3]; input Real Ri[3,3]; input Real pj[3]; input Real Rj[3,3];
  input Real translation[3]; input Real measuredRotation[3,3];
  output Real residual[6]; output Real Ji[6,6]; output Real Jj[6,6]; output Boolean valid;
protected Real t[3]; Real r[3]; Real square; Real theta; Real c;
  Real S[3,3]; Real leftInverse[3,3]; Real rightInverse[3,3];
algorithm
  t := PGMatVec(PGTranspose(Ri),pj-pi);
  (r,valid) := PGLog(PGMultiply(PGMultiply(PGTranspose(measuredRotation),PGTranspose(Ri)),Rj));
  square := r[1]*r[1]+r[2]*r[2]+r[3]*r[3]; theta := sqrt(max(square,0.0));
  c := if square < 1e-8 then 1.0/12.0+square/720.0+square*square/30240.0
    else (1.0-0.5*theta*cos(0.5*theta)/max(sin(0.5*theta),1e-12))/max(square,1e-12);
  S := PGSkew(r); leftInverse := identity(3)-0.5*S+c*PGMultiply(S,S);
  rightInverse := identity(3)+0.5*S+c*PGMultiply(S,S);
  residual := zeros(6); Ji := zeros(6,6); Jj := zeros(6,6); S := PGSkew(t);
  leftInverse := -PGMultiply(leftInverse,PGTranspose(measuredRotation));
  for i in 1:3 loop
    residual[i] := t[i]-translation[i]; residual[i+3] := r[i];
    for j in 1:3 loop
      Ji[i,j] := -Ri[j,i]; Jj[i,j] := Ri[j,i]; Ji[i,j+3] := S[i,j];
      Ji[i+3,j+3] := leftInverse[i,j]; Jj[i+3,j+3] := rightInverse[i,j];
    end for;
  end for;
end PGEdge;

function PGCholesky
  input Real A[6,6]; input Real pivotRelative;
  output Real L[6,6]; output Boolean valid;
protected Real scale; Real value;
algorithm
  L := zeros(6,6); valid := true; scale := 0.0; value := 0.0;
  for i in 1:6 loop scale := max(scale,abs(A[i,i])); end for;
  valid := scale > 1e-12 and scale <= 1e18;
  for i in 1:6 loop
    for j in 1:6 loop
      valid := valid and abs(A[i,j]) <= 1e18
        and abs(A[i,j]-A[j,i]) <= 1e-10*max(1.0,scale);
      value := A[i,j];
      for k in 1:6 loop
        if j <= i and k < j then value := value-L[i,k]*L[j,k]; end if;
      end for;
      if j <= i then
        if i == j then
          valid := valid and value > pivotRelative*scale and value <= 1e18;
          L[i,j] := sqrt(if value > pivotRelative*scale and value <= 1e18 then value else 1.0);
        else
          L[i,j] := value/L[j,j];
        end if;
      end if;
    end for;
  end for;
end PGCholesky;

function PGSolveBlock
  input Real L[6,6]; input Real b[6]; output Real x[6];
protected Real z[6]; Real value; Integer row;
algorithm
  z := zeros(6); x := zeros(6); value := 0.0; row := 1;
  for i in 1:6 loop
    value := b[i];
    for k in 1:6 loop if k < i then value := value-L[i,k]*z[k]; end if; end for;
    z[i] := value/L[i,i];
  end for;
  for reverseRow in 1:6 loop
    row := 7-reverseRow; value := z[row];
    for k in 1:6 loop if k > row then value := value-L[k,row]*x[k]; end if; end for;
    x[row] := value/L[row,row];
  end for;
end PGSolveBlock;

function PGNormalProduct
  input Real x[:,6]; input Real nodeMask[size(x,1)];
  input Real edgeMask[:]; input Integer source[size(edgeMask,1)]; input Integer target[size(edgeMask,1)];
  input Real Ji[size(edgeMask,1),6,6]; input Real Jj[size(edgeMask,1),6,6];
  input Real information[size(edgeMask,1),6,6]; input Real dampingDiagonal[size(x,1),6]; input Real damping;
  output Real y[size(x,1),6];
protected Integer i; Integer j; Real row[6]; Real weighted[6];
algorithm
  y := zeros(size(x,1),6); i := 1; j := 1; row := zeros(6); weighted := zeros(6);
  for node in 1:size(x,1) loop
    for k in 1:6 loop
      y[node,k] := if node > 1 and nodeMask[node] == 1.0 then damping*dampingDiagonal[node,k]*x[node,k] else 0.0;
    end for;
  end for;
  for edge in 1:size(edgeMask,1) loop
    if edgeMask[edge] == 1.0 then
      i := source[edge]; j := target[edge];
      row := Ji[edge,:,:]*x[i,:]+Jj[edge,:,:]*x[j,:]; weighted := information[edge,:,:]*row;
      if i > 1 then y[i,:] := y[i,:]+transpose(Ji[edge,:,:])*weighted; end if;
      if j > 1 then y[j,:] := y[j,:]+transpose(Jj[edge,:,:])*weighted; end if;
    end if;
  end for;
  // Node1 is the exact gauge: callers keep x[1,:]=0; its operator row is zero.
end PGNormalProduct;

function PGGraphCost
  input Real p[:,3]; input Real R[size(p,1),3,3]; input Real edgeMask[:];
  input Integer source[size(edgeMask,1)]; input Integer target[size(edgeMask,1)];
  input Real translation[size(edgeMask,1),3]; input Real measuredRotation[size(edgeMask,1),3,3];
  input Real information[size(edgeMask,1),6,6];
  output Real cost; output Boolean valid;
protected Real residual[6]; Real Ji[6,6]; Real Jj[6,6]; Boolean edgeValid;
algorithm
  cost := 0.0; valid := true; edgeValid := false; residual := zeros(6); Ji := zeros(6,6); Jj := zeros(6,6);
  for edge in 1:size(edgeMask,1) loop
    if edgeMask[edge] == 1.0 then
      (residual,Ji,Jj,edgeValid) := PGEdge(p[source[edge],:],R[source[edge],:,:],
        p[target[edge],:],R[target[edge],:,:],translation[edge,:],measuredRotation[edge,:,:]);
      valid := valid and edgeValid; cost := cost+0.5*(residual*(information[edge,:,:]*residual));
    end if;
  end for;
  valid := valid and cost >= 0.0 and cost <= 1e100;
end PGGraphCost;

function PGValidateGraph
  input Real p[:,3]; input Real R[size(p,1),3,3]; input Real nodeMask[size(p,1)]; input Real edgeMask[:];
  input Real fromNode[size(edgeMask,1)]; input Real toNode[size(edgeMask,1)];
  input Real translation[size(edgeMask,1),3]; input Real measuredRotation[size(edgeMask,1),3,3];
  input Real information[size(edgeMask,1),6,6];
  output Integer source[size(edgeMask,1)]; output Integer target[size(edgeMask,1)];
  output Real status; output Real activeNodes; output Real activeEdges;
protected Boolean valid; Boolean proper; Boolean endpoints; Boolean factorValid;
  Real L[6,6]; Real reached[size(p,1)]; Integer i; Integer j;
algorithm
  source := fill(1,size(edgeMask,1)); target := fill(1,size(edgeMask,1)); reached := zeros(size(p,1));
  valid := size(p,1) >= 1 and size(p,1) <= 128 and size(edgeMask,1) >= 1 and size(edgeMask,1) <= 256 and nodeMask[1] == 1.0;
  proper := false; endpoints := false; factorValid := false; L := zeros(6,6); i := 1; j := 1;
  status := -1.0; activeNodes := 0.0; activeEdges := 0.0;
  for node in 1:size(p,1) loop
    valid := valid and (nodeMask[node] == 0.0 or nodeMask[node] == 1.0);
    for k in 1:3 loop valid := valid and (nodeMask[node] == 0.0 or abs(p[node,k]) <= 1e6); end for;
    proper := PGProperRotation(R[node,:,:]); valid := valid and (nodeMask[node] == 0.0 or proper);
    activeNodes := activeNodes+(if nodeMask[node] == 1.0 then 1.0 else 0.0);
  end for;
  status := if valid then -2.0 else status;
  for edge in 1:size(edgeMask,1) loop
    endpoints := edgeMask[edge] == 1.0 and fromNode[edge] >= 1.0 and fromNode[edge] <= size(p,1)
      and floor(fromNode[edge]) == fromNode[edge] and toNode[edge] >= 1.0 and toNode[edge] <= size(p,1)
      and floor(toNode[edge]) == toNode[edge] and fromNode[edge] <> toNode[edge];
    valid := valid and (edgeMask[edge] == 0.0 or (edgeMask[edge] == 1.0 and endpoints));
    source[edge] := if endpoints then integer(fromNode[edge]) else 1;
    target[edge] := if endpoints then integer(toNode[edge]) else 1;
    valid := valid and (edgeMask[edge] == 0.0 or (nodeMask[source[edge]] == 1.0 and nodeMask[target[edge]] == 1.0));
    for k in 1:3 loop valid := valid and (edgeMask[edge] == 0.0 or abs(translation[edge,k]) <= 1e6); end for;
    proper := PGProperRotation(measuredRotation[edge,:,:]);
    (L,factorValid) := PGCholesky(information[edge,:,:],1e-10);
    valid := valid and (edgeMask[edge] == 0.0 or (proper and factorValid));
    activeEdges := activeEdges+(if edgeMask[edge] == 1.0 then 1.0 else 0.0);
  end for;
  status := if valid then -3.0 else status; reached[1] := 1.0;
  for pass in 1:size(p,1) loop
    for edge in 1:size(edgeMask,1) loop
      if valid and edgeMask[edge] == 1.0 then
        i := source[edge]; j := target[edge];
        if reached[i] == 1.0 or reached[j] == 1.0 then reached[i] := 1.0; reached[j] := 1.0; end if;
      end if;
    end for;
  end for;
  for node in 1:size(p,1) loop valid := valid and (nodeMask[node] == 0.0 or reached[node] == 1.0); end for;
  status := if valid then 1.0 else status;
end PGValidateGraph;

function PGLinearize
  input Real p[:,3]; input Real R[size(p,1),3,3]; input Real edgeMask[:];
  input Integer source[size(edgeMask,1)]; input Integer target[size(edgeMask,1)];
  input Real translation[size(edgeMask,1),3]; input Real measuredRotation[size(edgeMask,1),3,3];
  input Real information[size(edgeMask,1),6,6];
  output Real Ji[size(edgeMask,1),6,6]; output Real Jj[size(edgeMask,1),6,6];
  output Real gradient[size(p,1),6]; output Real blocks[size(p,1),6,6];
protected Integer i; Integer j; Real residual[6]; Boolean valid;
  Real edgeJi[6,6]; Real edgeJj[6,6];
algorithm
  Ji := zeros(size(edgeMask,1),6,6); Jj := zeros(size(edgeMask,1),6,6);
  gradient := zeros(size(p,1),6); blocks := zeros(size(p,1),6,6); residual := zeros(6); valid := false; i := 1; j := 1;
  edgeJi := zeros(6,6); edgeJj := zeros(6,6);
  for edge in 1:size(edgeMask,1) loop
    if edgeMask[edge] == 1.0 then
      i := source[edge]; j := target[edge];
      (residual,edgeJi,edgeJj,valid) := PGEdge(p[i,:],R[i,:,:],p[j,:],R[j,:,:],translation[edge,:],measuredRotation[edge,:,:]);
      Ji[edge,:,:] := edgeJi; Jj[edge,:,:] := edgeJj;
      if i > 1 then
        gradient[i,:] := gradient[i,:]+transpose(Ji[edge,:,:])*(information[edge,:,:]*residual);
        blocks[i,:,:] := blocks[i,:,:]+transpose(Ji[edge,:,:])*information[edge,:,:]*Ji[edge,:,:];
      end if;
      if j > 1 then
        gradient[j,:] := gradient[j,:]+transpose(Jj[edge,:,:])*(information[edge,:,:]*residual);
        blocks[j,:,:] := blocks[j,:,:]+transpose(Jj[edge,:,:])*information[edge,:,:]*Jj[edge,:,:];
      end if;
    end if;
  end for;
end PGLinearize;

function PGDampedFactor
  input Real localBlock[6,6]; input Real damping;
  output Real L[6,6]; output Real diagonal[6]; output Boolean valid;
protected Real A[6,6];
algorithm
  A := localBlock; diagonal := zeros(6);
  for k in 1:6 loop diagonal[k] := max(A[k,k],1e-6); A[k,k] := A[k,k]+damping*diagonal[k]; end for;
  (L,valid) := PGCholesky(A,1e-12);
end PGDampedFactor;

function PGPrecondition
  input Real rhs[:,6]; input Real factors[size(rhs,1),6,6]; input Real nodeMask[size(rhs,1)];
  output Real z[size(rhs,1),6];
algorithm
  z := zeros(size(rhs,1),6);
  for node in 1:size(rhs,1) loop
    if node > 1 and nodeMask[node] == 1.0 then z[node,:] := PGSolveBlock(factors[node,:,:],rhs[node,:]); end if;
  end for;
end PGPrecondition;

function PGDot
  input Real x[:,:]; input Real y[size(x,1),size(x,2)]; output Real value;
algorithm
  value := 0.0;
  for i in 1:size(x,1) loop for j in 1:size(x,2) loop value := value+x[i,j]*y[i,j]; end for; end for;
end PGDot;

function PGPCG
  input Real gradient[:,6]; input Real blocks[size(gradient,1),6,6]; input Real nodeMask[size(gradient,1)];
  input Real edgeMask[:]; input Integer source[size(edgeMask,1)]; input Integer target[size(edgeMask,1)];
  input Real Ji[size(edgeMask,1),6,6]; input Real Jj[size(edgeMask,1),6,6]; input Real information[size(edgeMask,1),6,6];
  input Real damping; input Real tolerance; input Integer maximumPCG;
  output Real delta[size(gradient,1),6]; output Real iterations; output Real initialNorm; output Boolean valid;
protected Real factors[size(gradient,1),6,6]; Real diagonal[size(gradient,1),6]; Real rhs[size(gradient,1),6];
  Real localFactor[6,6]; Real localDiagonal[6];
  Real z[size(gradient,1),6]; Real direction[size(gradient,1),6]; Real product[size(gradient,1),6];
  Real rho; Real nextRho; Real denominator; Real alpha; Real beta; Real residualNorm; Boolean factorValid; Boolean running;
algorithm
  factors := zeros(size(gradient,1),6,6); diagonal := zeros(size(gradient,1),6); delta := zeros(size(gradient,1),6);
  localFactor := zeros(6,6); localDiagonal := zeros(6);
  rhs := -gradient; z := zeros(size(gradient,1),6); direction := zeros(size(gradient,1),6); product := zeros(size(gradient,1),6);
  valid := true; factorValid := false; iterations := 0.0; rho := 0.0; nextRho := 0.0; denominator := 0.0;
  alpha := 0.0; beta := 0.0; residualNorm := 0.0; running := false;
  for node in 1:size(gradient,1) loop
    if node > 1 and nodeMask[node] == 1.0 then
      (localFactor,localDiagonal,factorValid) := PGDampedFactor(blocks[node,:,:],damping);
      factors[node,:,:] := localFactor; diagonal[node,:] := localDiagonal;
      valid := valid and factorValid;
    end if;
  end for;
  z := PGPrecondition(rhs,factors,nodeMask); direction := z;
  rho := PGDot(rhs,z); initialNorm := PGDot(rhs,rhs);
  valid := valid and rho >= 0.0 and rho <= 1e100 and initialNorm <= 1e100;
  running := valid and initialNorm > 1e-18;
  for pcg in 1:96 loop
    if running and pcg <= maximumPCG then
      iterations := iterations+1.0;
      product := PGNormalProduct(direction,nodeMask,edgeMask,source,target,Ji,Jj,information,diagonal,damping);
      denominator := PGDot(direction,product); valid := denominator > 0.0 and denominator <= 1e100 and rho > 0.0;
      if valid then
        alpha := rho/denominator; delta := delta+alpha*direction; rhs := rhs-alpha*product; residualNorm := PGDot(rhs,rhs);
        valid := residualNorm >= 0.0 and residualNorm <= 1e100;
        running := valid and residualNorm > tolerance*tolerance*initialNorm;
        if running then
          z := PGPrecondition(rhs,factors,nodeMask); nextRho := PGDot(rhs,z);
          valid := nextRho > 0.0 and nextRho <= 1e100; beta := nextRho/max(rho,1e-300);
          direction := z+beta*direction; rho := nextRho; running := running and valid;
        end if;
      else running := false;
      end if;
    end if;
  end for;
end PGPCG;

function PGRetract
  input Real p[:,3]; input Real R[size(p,1),3,3]; input Real nodeMask[size(p,1)]; input Real delta[size(p,1),6]; input Real scale;
  output Real nextP[size(p,1),3]; output Real nextR[size(p,1),3,3];
algorithm
  nextP := p; nextR := R;
  for node in 1:size(p,1) loop
    if node > 1 and nodeMask[node] == 1.0 then
      nextP[node,:] := p[node,:]+scale*delta[node,1:3]; nextR[node,:,:] := R[node,:,:]*PGExp(scale*delta[node,4:6]);
    end if;
  end for;
end PGRetract;

function PGStep
  input Real p[:,3]; input Real R[size(p,1),3,3]; input Real nodeMask[size(p,1)]; input Real edgeMask[:];
  input Integer source[size(edgeMask,1)]; input Integer target[size(edgeMask,1)];
  input Real translation[size(edgeMask,1),3]; input Real measuredRotation[size(edgeMask,1),3,3]; input Real information[size(edgeMask,1),6,6];
  input Real currentCost; input Real damping; input Real maximumPositionStep; input Real maximumAngleStep;
  input Real pcgTolerance; input Integer maximumPCG; input Integer maximumBacktracks;
  output Real nextP[size(p,1),3]; output Real nextR[size(p,1),3,3]; output Real nextCost; output Real nextDamping;
  output Real accepted; output Real iterations; output Boolean running;
protected Real Ji[size(edgeMask,1),6,6]; Real Jj[size(edgeMask,1),6,6]; Real gradient[size(p,1),6]; Real blocks[size(p,1),6,6];
  Real delta[size(p,1),6]; Real initialNorm; Real scale; Real positionNorm; Real angleNorm;
  Real trialP[size(p,1),3]; Real trialR[size(p,1),3,3]; Real trialCost; Boolean pcgValid; Boolean trialValid;
algorithm
  nextP := p; nextR := R; nextCost := currentCost; nextDamping := min(1e6,damping*10.0); accepted := 0.0;
  trialP := p; trialR := R; trialCost := currentCost; scale := 1.0; positionNorm := 0.0; angleNorm := 0.0; trialValid := false;
  (Ji,Jj,gradient,blocks) := PGLinearize(p,R,edgeMask,source,target,translation,measuredRotation,information);
  (delta,iterations,initialNorm,pcgValid) := PGPCG(gradient,blocks,nodeMask,edgeMask,source,target,Ji,Jj,information,damping,pcgTolerance,maximumPCG);
  running := initialNorm > 1e-18;
  for node in 1:size(p,1) loop
    positionNorm := sqrt(max(delta[node,1:3]*delta[node,1:3],0.0)); angleNorm := sqrt(max(delta[node,4:6]*delta[node,4:6],0.0));
    scale := min(scale,min(maximumPositionStep/max(positionNorm,1e-12),maximumAngleStep/max(angleNorm,1e-12)));
  end for;
  for backtrack in 1:12 loop
    if pcgValid and running and accepted == 0.0 and backtrack <= maximumBacktracks then
      (trialP,trialR) := PGRetract(p,R,nodeMask,delta,scale);
      (trialCost,trialValid) := PGGraphCost(trialP,trialR,edgeMask,source,target,translation,measuredRotation,information);
      if trialValid and trialCost < currentCost-1e-12*max(1.0,currentCost) then
        nextP := trialP; nextR := trialR; nextCost := trialCost; nextDamping := max(1e-9,damping*0.3); accepted := 1.0;
      else scale := scale*0.5;
      end if;
    end if;
  end for;
end PGStep;

function PGRun
  input Real p[:,3]; input Real R[size(p,1),3,3]; input Real nodeMask[size(p,1)]; input Real edgeMask[:];
  input Integer source[size(edgeMask,1)]; input Integer target[size(edgeMask,1)];
  input Real translation[size(edgeMask,1),3]; input Real measuredRotation[size(edgeMask,1),3,3]; input Real information[size(edgeMask,1),6,6];
  input Real initialCost; input Real initialDamping; input Real maximumPositionStep; input Real maximumAngleStep; input Real pcgTolerance;
  input Integer maximumIterations; input Integer maximumPCG; input Integer maximumBacktracks;
  output Real nextP[size(p,1),3]; output Real nextR[size(p,1),3,3]; output Real cost; output Real acceptedIterations; output Real pcgIterations;
protected Real damping; Real accepted; Real iterations; Boolean running;
algorithm
  nextP := p; nextR := R; cost := initialCost; acceptedIterations := 0.0; pcgIterations := 0.0;
  damping := initialDamping; accepted := 0.0; iterations := 0.0; running := true;
  for iteration in 1:16 loop
    if running and iteration <= maximumIterations then
      (nextP,nextR,cost,damping,accepted,iterations,running) := PGStep(nextP,nextR,nodeMask,edgeMask,source,target,
        translation,measuredRotation,information,cost,damping,maximumPositionStep,maximumAngleStep,pcgTolerance,maximumPCG,maximumBacktracks);
      acceptedIterations := acceptedIterations+accepted; pcgIterations := pcgIterations+iterations;
    end if;
  end for;
end PGRun;

function OptimizeModelicaPoseGraph
  input Real position[:,3]; input Real rotation[size(position,1),3,3]; input Real nodeMask[size(position,1)];
  input Real edgeMask[:]; input Real fromNode[size(edgeMask,1)]; input Real toNode[size(edgeMask,1)];
  input Real translation[size(edgeMask,1),3]; input Real measuredRotation[size(edgeMask,1),3,3]; input Real information[size(edgeMask,1),6,6];
  input Integer maximumIterations; input Integer maximumPCG; input Integer maximumBacktracks;
  input Real initialDamping; input Real maximumPositionStep; input Real maximumAngleStep; input Real pcgTolerance;
  output Real nextPosition[size(position,1),3]; output Real nextRotation[size(position,1),3,3]; output Real status;
  output Real costBefore; output Real costAfter; output Real acceptedIterations; output Real pcgIterations; output Real activeNodes; output Real activeEdges;
protected Integer source[size(edgeMask,1)]; Integer target[size(edgeMask,1)]; Boolean valid; Boolean costValid;
algorithm
  nextPosition := position; nextRotation := rotation; costBefore := 0.0; costAfter := 0.0; acceptedIterations := 0.0; pcgIterations := 0.0;
  (source,target,status,activeNodes,activeEdges) := PGValidateGraph(position,rotation,nodeMask,edgeMask,fromNode,toNode,translation,measuredRotation,information);
  valid := status == 1.0 and maximumIterations >= 1 and maximumIterations <= 16 and maximumPCG >= 1 and maximumPCG <= 96
    and maximumBacktracks >= 1 and maximumBacktracks <= 12 and initialDamping >= 1e-9 and initialDamping <= 1e3
    and maximumPositionStep > 0.0 and maximumPositionStep <= 2.0 and maximumAngleStep > 0.0 and maximumAngleStep <= 0.5
    and pcgTolerance >= 1e-8 and pcgTolerance <= 0.1;
  status := if status == 1.0 and not valid then -1.0 else status; costValid := false;
  if valid then
    (costBefore,costValid) := PGGraphCost(position,rotation,edgeMask,source,target,translation,measuredRotation,information);
    status := if costValid then 1.0 else -4.0; costAfter := costBefore;
    if costValid then
      (nextPosition,nextRotation,costAfter,acceptedIterations,pcgIterations) := PGRun(position,rotation,nodeMask,edgeMask,source,target,
        translation,measuredRotation,information,costBefore,initialDamping,maximumPositionStep,maximumAngleStep,pcgTolerance,maximumIterations,maximumPCG,maximumBacktracks);
      status := if acceptedIterations > 0.0 then 2.0 else 1.0;
    end if;
  end if;
end OptimizeModelicaPoseGraph;

function PGEdgeDiagnostics
  input Real pi[3]; input Real Ri[3,3]; input Real pj[3]; input Real Rj[3,3]; input Real translation[3]; input Real measuredRotation[3,3];
  output Real residual[6]; output Real Ji[6,6]; output Real Jj[6,6]; output Real valid;
protected Boolean chartValid;
algorithm
  (residual,Ji,Jj,chartValid) := PGEdge(pi,Ri,pj,Rj,translation,measuredRotation); valid := if chartValid then 1.0 else 0.0;
end PGEdgeDiagnostics;

model ModelicaPoseGraph
  parameter Integer nodeCapacity = 128; parameter Integer edgeCapacity = 256;
  parameter Integer maximumIterations = 8; parameter Integer maximumPCG = 48; parameter Integer maximumBacktracks = 8;
  parameter Real initialDamping = 0.001; parameter Real maximumPositionStep = 0.5;
  parameter Real maximumAngleStep = 0.2; parameter Real pcgTolerance = 1e-5;
  input Real position[nodeCapacity,3] = zeros(nodeCapacity,3);
  input Real rotation[nodeCapacity,3,3] = zeros(nodeCapacity,3,3);
  input Real nodeMask[nodeCapacity] = zeros(nodeCapacity); input Real edgeMask[edgeCapacity] = zeros(edgeCapacity);
  input Real fromNode[edgeCapacity] = ones(edgeCapacity); input Real toNode[edgeCapacity] = ones(edgeCapacity);
  input Real translation[edgeCapacity,3] = zeros(edgeCapacity,3);
  input Real measuredRotation[edgeCapacity,3,3] = zeros(edgeCapacity,3,3);
  input Real information[edgeCapacity,6,6] = zeros(edgeCapacity,6,6);
  output Real nextPosition[nodeCapacity,3]; output Real nextRotation[nodeCapacity,3,3];
  output Real status; output Real costBefore; output Real costAfter; output Real acceptedIterations;
  output Real pcgIterations; output Real activeNodes; output Real activeEdges;
equation
  (nextPosition,nextRotation,status,costBefore,costAfter,acceptedIterations,pcgIterations,activeNodes,activeEdges) =
    OptimizeModelicaPoseGraph(position,rotation,nodeMask,edgeMask,fromNode,toNode,translation,measuredRotation,information,
      maximumIterations,maximumPCG,maximumBacktracks,initialDamping,maximumPositionStep,maximumAngleStep,pcgTolerance);
end ModelicaPoseGraph;

model ModelicaPoseGraphEdge
  input Real pi[3] = zeros(3); input Real pj[3] = zeros(3);
  input Real Ri[3,3] = identity(3); input Real Rj[3,3] = identity(3);
  input Real translation[3] = zeros(3); input Real measuredRotation[3,3] = identity(3);
  output Real residual[6]; output Real Ji[6,6]; output Real Jj[6,6]; output Real valid;
equation
  (residual,Ji,Jj,valid) = PGEdgeDiagnostics(pi,Ri,pj,Rj,translation,measuredRotation);
end ModelicaPoseGraphEdge;
