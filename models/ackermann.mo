model Ackermann "hello"
    parameter Real wheel_seperation = 0.1;
    parameter Real wheel_base = 0.2;
    parameter Real wheel_radius = 0.3;
    parameter Real wheel_width = 0.4;
    parameter Real wheel_mass = 0.5;
    parameter Real wheel_max_turn_angle = 0.6;
    
    parameter Real fuselage_mass = 0.7;
    parameter Real fuselage_width = 0.8;
    parameter Real fuselage_height = 0.9;
    parameter Real fuselage_length = 0.10;

    parameter Real wheel_max_rotational_rate = 1;
    parameter Real wheel_inertia_ixx = (wheel_mass / 2) * (wheel_radius ^ 2);
    output Real x;
    output Real y;
    input Real u;
    input Real omega;
equation
    der(x) = u*cos(theta);
    der(y) = u*sin(theta);
    der(theta) = omega;
end Ackermann;
