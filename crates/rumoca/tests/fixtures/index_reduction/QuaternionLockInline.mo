model QuaternionLockInline
  Real q[4](each fixed=false, each stateSelect=StateSelect.prefer, start={0,0,0,1});
  Real w[3](each fixed=false);
  Real a1, a2;
equation
  w = 2*{ q[4]*der(q[1]) + q[3]*der(q[2]) - q[2]*der(q[3]) - q[1]*der(q[4]),
         -q[3]*der(q[1]) + q[4]*der(q[2]) + q[1]*der(q[3]) - q[2]*der(q[4]),
          q[2]*der(q[1]) - q[1]*der(q[2]) + q[4]*der(q[3]) - q[3]*der(q[4])};
  der(w) = {a1, a2, 0};
  q*q = 1;
  0 = 2*(q[1]*q[2] - q[3]*q[4]);
  0 = 2*(q[1]*q[3] + q[2]*q[4]);
end QuaternionLockInline;
