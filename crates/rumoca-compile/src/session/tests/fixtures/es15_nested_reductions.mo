// World-additive p/v, right-local theta, body ba/bg: the ES15Dynamics convention.
// A held-sample prediction uses a cubic transition and positive three-node
// Gauss-Legendre process-noise quadrature, matching the current filter contract.
// Every array operation is Modelica; this proof kernel is not yet a runtime node.
model ES15CovariancePrediction
  input Real F[15,15] = fill(0.0,15,15);
  input Real G[15,12] = fill(0.0,15,12);
  input Real P[15,15] = identity(15);
  input Real dt = 1.0/90.0;
  input Real density[12] = {0.06,0.06,0.06,0.006,0.006,0.006,
                           0.002,0.002,0.002,0.0002,0.0002,0.0002};
  output Real Phi[15,15];
  output Real Q[15,15];
  output Real predicted[15,15];
protected
  parameter Real fraction[3] = {0.5-sqrt(15.0)/10.0,0.5,0.5+sqrt(15.0)/10.0};
  parameter Real weight[3] = {5.0/18.0,4.0/9.0,5.0/18.0};
  Real A[15,15]; Real A2[15,15]; Real A3[15,15];
  Real noiseTransition[3,15,15]; Real B[3,15,12];
  Real propagated[15,15]; Real raw[15,15];
equation
  A = F*dt;
  A2 = A*A;
  A3 = A2*A;
  Phi = identity(15)+A+0.5*A2+A3/6.0;
  for node in 1:3 loop
    for i in 1:15 loop
      for j in 1:15 loop
        noiseTransition[node,i,j] = (if i == j then 1.0 else 0.0)
          +fraction[node]*A[i,j]+0.5*fraction[node]^2*A2[i,j]
          +fraction[node]^3*A3[i,j]/6.0;
      end for;
      for j in 1:12 loop
        B[node,i,j] = sum(noiseTransition[node,i,k]*G[k,j] for k in 1:15)*density[j];
      end for;
    end for;
  end for;
  for i in 1:15 loop
    for j in 1:15 loop
      Q[i,j] = dt*sum(weight[node]*sum(B[node,i,k]*B[node,j,k] for k in 1:12) for node in 1:3);
    end for;
  end for;
  propagated = Phi*P*transpose(Phi);
  raw = propagated+Q;
  predicted = 0.5*(raw+transpose(raw));
end ES15CovariancePrediction;
