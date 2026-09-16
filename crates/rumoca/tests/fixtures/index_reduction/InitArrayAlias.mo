model InitArrayAlias
  Real z[2](each fixed=false);
  Real q[3], w[3];
initial equation
  q[2]=0; w[2]=-2.4;
equation
  q={0,z[1],0}; w={0,z[2],0};
  der(z)={w[2],0};
end InitArrayAlias;
