model Ackermann "hello"
    parameter Real wheel_seperation = 1;
    parameter Real wheel_base = 1;
    parameter Real wheel_radius = 1;
    parameter Real wheel_width = 1;
    parameter Real wheel_mass = 1;
    parameter Real wheel_max_turn_angle = 1;
    
    parameter Real fuselage_mass = 1;
    parameter Real fuselage_width = 1;
    parameter Real fuselage_height = 1;
    parameter Real fuselage_length = 1;

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
