model SimpleCircuit
    Resistor R1(R=10);
    Capacitor C(C=0.01);
    Resistor R2(R=100);
    Inductor L(L=0.1);
    VsourceAC AC;
    Ground G;
equation
    connect(AC.p, R1.p);  // Wire 1, Capacitor circuit
    connect(R1.n, C.p);   // Wire 2
    connect(C.n, Ac.n);   // Wire 3
    connect(R1.p, R2.p);  // Wire 4, Inductor circuit
    connect(R2.n, L.p);   // Wire 5
    connect(L.n, C.n);    // Wire 6
    connect(AC.n, G.p);   // Wire 7, Ground
end SimpleCircuit;

partial class TwoPin
    Pin p, n;
    Real v;
    Real i;
equation
    v = p.v - n.v;
    0 = p.i + n.i;
    i = p.i;
end TwoPin;

class Resistor
    extends TwoPin;
    parameter Real R;
equation
    R*i = v;
end Resistor;

class Inductor "Ideal electrical inductor"
    extends TwoPin;
    parameter Real L "Inductance";
equation
    v = L*der(i);
end Inductor;

class Capacitor
    extends TwoPin;
    parameter Real C;
equation
    C*der(v) = i;
end Capacitor;

class Ground
    Pin p;
equation
    p.v = 0;
end Ground;

class VsourceAC "Sin-wave voltage source"
    extends TwoPin;
    parameter Real VA = 220 "Amplitude";
    parameter Real f = 50 "Frequency";
    constant Real PI = 3.14159;
equation
    v = VA*sin(2*PI*f*time);
end VsourceAC;