model CardanChartFold
  "Angle a2 is solved from a universal-joint-type constraint in the Cardan chart (a1, a3); the chart is singular where cos(a3) = 0, which the exact solution crosses at t = 0.5 with a1 = 0"
  parameter Real k = sin(0.3);
  parameter Real a3_0 = -3.141592653589793/2 - 0.5;
  Real a1(start = atan(k*cos(a3_0)/sin(a3_0)), fixed = true);
  Real a3(start = a3_0, fixed = true);
  Real a2(start = 0.3);
  Real z(start = 0, fixed = true);
equation
  der(a3) = 1;
  der(a1) = -k/(sin(a3)^2 + k^2*cos(a3)^2);
  sin(a3)*sin(a1) = cos(a3)*sin(a2)*cos(a1);
  der(z) = a2;
end CardanChartFold;
