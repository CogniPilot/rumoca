model SplitCircleChart
  Real x(start = 1);
  Real y(start = 0, fixed = true);
  Real vx(start = 0);
  Real vy(start = 1, fixed = true);
  Real lambda;
equation
  der(x) = vx;
  der(y) = vy;
  der(vx) = lambda*x;
  der(vy) = lambda*y;
  x*x + y*y = 1;
end SplitCircleChart;
