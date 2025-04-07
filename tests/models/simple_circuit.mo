class SimpleCircuit
    Resistor R1(R=10);
    Capacitor C(C=0.01);
    Resistor R2(R=100);
    Inductor L1(L=0.1);
    VsourceAC AC;
    Ground G;
equation
    connect(AC.p, R1.p); // 1, Capacitor circuit
    connect(R1.n, C.p);  //    Wire 2
    connect(C.n, AC.n);  //    Wire 3
    connect(R1.p, R2.p); // 2, Inductor circuit
    connect(R2.n, L.p);  //    Wire 5
    connect(L.n, C.n);   //    Wire 6
    connect(AC.n, G.p);  // 7, Ground
end SimpleCircuit;

//type Voltage = Real(unit="V") "Voltage";
//type Current = Real(unit="A") "Current";

connector Pin
    Real v;
    flow Current i;
end Pin;

partial class TwoPin "Superclass of elements with two electical pins"
    Pin p, n;
    Voltage v;
    Current i;
equation
    v = p.v - n.v;
    0 = p.i + n.i;
    i = p.i;
end TwoPin;

class Ground "Ground"
    Pin p;
equation
    p.v = 0;
end Ground;

class Resistor "Ideal electrical resistor"
    extends TwoPin;
    parameter Real R(unit="Ohm") "Resistance";
equation
    R*i = v;
end Resistor;

class Capacitor "Ideal electrical capacitor"
    extends TwoPin;
    parameter Real C(unit="F") "Capacitance";
equation
    C*der(v) = i;
end Capacitor;

class Inductor "Ideal electrical inductor"
    extends TwoPin;
    parameter Real L(unit="H") "Inductance";
equation
    L*der(i) = v;
end Inductor;

class VsourceAC "Sin wave voltage source"
    extends TwoPin;
    parameter Voltage VA = 220 "Amplitude";
    parameter Real f(unit="Hz") = 50 "Frequency";
    parameter Real PI = 3.14159265358979323846 "Pi";
equation
    v = VA*sin(2*PI*f*time);
end VsourceAC;