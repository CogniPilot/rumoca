model Ball
  Real x(start=10);
  Real v(start=1);
  parameter Real g = 9.81;
equation
  der(x) = v;
  der(v) = -g;
  when x < 0 then
    //terminate("Ball has hit the ground");
    reinit(v, -0.8*pre(v));
  end when;
end Ball;
