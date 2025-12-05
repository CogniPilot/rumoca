// Test fixtures for connector balance checking
package Connectors
  // Simple electrical connector
  connector Pin
    Real v "Potential";
    flow Real i "Current";
  end Pin;

  // A simple resistor with two pins
  // When connected: 4 unknowns (p.v, p.i, n.v, n.i), 2 internal equations + 2 from connections
  model Resistor
    parameter Real R = 1.0;
    Pin p "Positive pin";
    Pin n "Negative pin";
  equation
    // Ohm's law: voltage across = R * current through
    p.v - n.v = R * p.i;
    // Current conservation: what goes in comes out
    p.i + n.i = 0;
  end Resistor;

  // Simple voltage source
  model VoltageSource
    parameter Real V = 1.0;
    Pin p "Positive pin";
    Pin n "Negative pin";
  equation
    p.v - n.v = V;
    p.i + n.i = 0;
  end VoltageSource;

  // Ground - sets voltage reference
  model Ground
    Pin p;
  equation
    p.v = 0;
  end Ground;

  // Simple RC circuit - should be balanced when connections are expanded
  // Components: V (source), R (resistor), C (capacitor), G (ground)
  // Connections provide the missing equations
  model SimpleRCCircuit
    parameter Real R = 1000.0;
    parameter Real C = 1e-6;
    parameter Real V = 5.0;
    VoltageSource source(V = V);
    Resistor resistor(R = R);
    Ground ground;
    // Capacitor inline
    Real vc(start = 0) "Capacitor voltage";
    Real ic "Capacitor current";
  equation
    // Capacitor equation
    C * der(vc) = ic;
    // Connections
    connect(source.p, resistor.p);
    connect(resistor.n, ground.p);
    // Simplified: capacitor between resistor and ground
    connect(source.n, ground.p);
    // Capacitor connected in parallel with resistor output
    vc = resistor.n.v - ground.p.v;
    ic = -resistor.n.i;
  end SimpleRCCircuit;

  // Current into capacitor
  // Very simple: two resistors in series with ground
  // Should have: 6 pin variables, connections generate equality + flow equations
  model TwoResistors
    Resistor R1(R = 100);
    Resistor R2(R = 200);
    Ground G;
    Pin p "External connection point";
  equation
    connect(p, R1.p);
    connect(R1.n, R2.p);
    connect(R2.n, G.p);
  end TwoResistors;

  // Minimal test: single resistor with external pins
  model SingleResistor
    Resistor R(R = 100);
    Pin p;
    Pin n;
  equation
    connect(p, R.p);
    connect(n, R.n);
  end SingleResistor;

  // Test for causal blocks with input/output
  // Similar to SampleVectorizedAndClocked pattern
  connector RealInput = input Real;

  connector RealOutput = output Real;

  // Simple SISO block - should be balanced (1 eq, 1 unk: output y)
  block SimpleGain
    parameter Real k = 1.0;
    RealInput u;
    RealOutput y;
  equation
    y = k * u;
  end SimpleGain;

  // Vectorized block with parameter-sized arrays
  // With n=2: should be 2 eq, 2 unk (y[1], y[2])
  block VectorizedGain
    parameter Integer n = 2;
    parameter Real k = 1.0;
    RealInput u[n];
    RealOutput y[n];
  equation
    y = k * u;
  end VectorizedGain;

  // Minimal test: just output variable with direct type
  block JustOutput
    output Real y;
  equation
    y = 1.0;
  end JustOutput;

  // Test with type alias
  block JustOutputAlias
    RealOutput y;
  equation
    y = 1.0;
  end JustOutputAlias;

  // Test for empty arrays (n=0) - should be balanced (0 eq, 0 unk)
  block EmptyArrayPassthrough
    parameter Integer n = 0;
    RealInput u[n];
    RealOutput y[n];
  equation
    y = u;
  end EmptyArrayPassthrough;
end Connectors;
