model BouncingBall "The 'classic' bouncing ball model"
  parameter Real e=0.8 "Coefficient of restitution";
  parameter Real h0=1.0 "Initial height";
  Real h = 5.0 "Height";
  Real v "Velocity";
  Real z;
equation
  z = 2*h + v;
  der(h) = v;
  der(v) = -9.81;
  when h<0 then
    reinit(v, -e*pre(v));
  end when;

end BouncingBall;