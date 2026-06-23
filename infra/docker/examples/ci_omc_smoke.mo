model DockerSmoke
  Real x(start = 1);
equation
  der(x) = -x;
end DockerSmoke;
