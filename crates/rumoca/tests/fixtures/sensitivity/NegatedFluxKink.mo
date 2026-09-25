package NegatedFluxKink
  function muApprox "Relative permeability over the normalized flux density"
    input Real BN;
    output Real mu;
  algorithm
    mu := 1 + 1 / (1 + BN);
  end muApprox;

  model Loop
    parameter Real N = 100;
    Real i(start = 0, fixed = true);
    Real Phi1;
    Real Phi2;
    Real Vm1;
    Real Vm2;
    Real BN1;
    Real BN2;
    Real mu1;
    Real mu2;
  equation
    der(i) = max(time - 0.5, 0) - 0.01 * Phi1;
    Phi1 + Phi2 = 0;
    BN1 = abs(Phi1 / 2);
    BN2 = abs(Phi2 / 2);
    mu1 = muApprox(BN1);
    mu2 = muApprox(BN2);
    Vm1 = Phi1 / mu1;
    Vm2 = -Phi2 / mu2;
    Vm1 + Vm2 = N * i;
  end Loop;
end NegatedFluxKink;
