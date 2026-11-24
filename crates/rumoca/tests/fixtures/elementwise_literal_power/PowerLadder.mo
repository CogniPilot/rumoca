package PowerLadder "N-section RLC ladder with cubic shunt conductances, in array equations"
  model Ladder
    parameter Integer N = 10 "number of sections";
    parameter Real R = 0.5 "series resistance";
    parameter Real L = 1e-3 "series inductance";
    parameter Real C = 1e-6 "node capacitance";
    parameter Real G = 1e-3 "linear shunt conductance";
    parameter Real k = 1e-6 "cubic shunt conductance";
    parameter Real Rm = 10 "resistance between a capacitor and its node";
    parameter Real f = 60 "source frequency";
    parameter Real Vs = 100 "source amplitude";
    Real iL[N](each start = 0, each fixed = true) "series inductor currents";
    Real vC[N](each start = 0, each fixed = true) "capacitor voltages";
    Real v[N] "node voltages (algebraic)";
    Real vs "source voltage";
  equation
    vs = Vs * sin(2 * 3.141592653589793 * f * time);
    L * der(iL) = cat(1, {vs}, v[1:N - 1]) - v - R * iL;
    // KCL at every node: inductor in, next inductor out, capacitor branch, cubic shunt.
    iL - cat(1, iL[2:N], {0}) = (v - vC) / Rm + G * v + k * v .^ 3;
    C * der(vC) = (v - vC) / Rm;
  end Ladder;

  model Ladder50 = Ladder(N = 50);
end PowerLadder;
