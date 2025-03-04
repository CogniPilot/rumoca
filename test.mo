// To run the test please issue:
// cargo run ./test.mo

within stuff.test1.test2;

class Test2
    Real x, y, z;
    parameter Real w;
equation
    x = 3.0;
algorithm
    y := x[1, 2]*6 + 10^7;
    z := der(2);
end Test2;