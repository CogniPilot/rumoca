// Test replaceable package type resolution
// This tests that Medium.X can resolve to the parent package of PartialMedium
// Top-level package (simulates Modelica.Media)
package Media
  // Nested Interfaces package (simulates Modelica.Media.Interfaces)
  package Interfaces
    // A type defined in the package (not in PartialMedium)
    type MassFraction = Real(min = 0, max = 1);

    type AbsolutePressure = Real(min = 0);

    type Temperature = Real(min = 0);

    // Partial medium package that types inherit from
    package PartialMedium
      // PartialMedium doesn't define MassFraction, AbsolutePressure, etc.
      // They are defined in the parent package Interfaces
      constant Integer nXi = 2;
    end PartialMedium;
  end Interfaces;
end Media;

// Package for fluid components (simulates Modelica.Fluid)
package Fluid
  package Examples
    package BatchPlant
      package BaseClasses
        // A model that uses replaceable package Medium (like InnerTank)
        model TankWithMedium
          // These types should resolve to Media.Interfaces.MassFraction, etc.
          // not Media.Interfaces.PartialMedium.MassFraction (which doesn't exist)
          Medium.MassFraction Xi[Medium.nXi];
          Medium.AbsolutePressure p;
          Medium.Temperature T;
          package Medium = Media.Interfaces.PartialMedium;
        equation
          Xi = {0.5, 0.5};
          p = 101325;
          T = 300;
        end TankWithMedium;
      end BaseClasses;
    end BatchPlant;
  end Examples;
end Fluid;
