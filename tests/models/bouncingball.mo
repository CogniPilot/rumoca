 model BouncingBall "bouncing ball"
    constant Real g = 9.81;
    parameter Real c = 0.9;
    parameter Real radius = 0.1;
    Real height = 1;
    Real velocity = 0;
equation
    der(height) = velocity;
    der(velocity) = -g;
    //when height <= radius then
    //    reinit(velocity, -c*pre(velocity));
    //end when;
end BouncingBall;