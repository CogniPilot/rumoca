// Test SI unit conversions from MSL
model SIConversionTest
  import Modelica.Units.Conversions.from_degC;
  import Modelica.Units.Conversions.to_degC;
  parameter Real T_celsius = 25.0 "Temperature in Celsius";
  Real T_kelvin "Temperature in Kelvin";
  Real T_back "Temperature converted back to Celsius";
equation
  T_kelvin = from_degC(T_celsius);
  T_back = to_degC(T_kelvin);
end SIConversionTest;
