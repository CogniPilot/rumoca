model OrderedSweep
  Real q[3,5];
  output Real y[3];
equation
  for i in 1:3 loop q[i,1] = 0; end for;
  for k in 1:4 loop
    for i in 1:3 loop
      q[i,k+1] = q[i,k] + i +
        sum(q[j, if j<i then k+1 else k] for j in 1:3);
    end for;
  end for;
  for i in 1:3 loop y[i] = q[i,5]; end for;
end OrderedSweep;
