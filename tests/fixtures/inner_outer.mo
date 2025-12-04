// Test inner/outer component parsing
model World "Global coordinate system"
  parameter Real g = 9.81 "Gravitational acceleration";
end World;

model InnerOuterTest "Test model with inner/outer"
  inner World world;  // Provides World instance to children
end InnerOuterTest;

model ChildModel "Model that uses outer reference"
  outer World world;  // References World from enclosing scope
  Real v;
equation
  v = world.g;
end ChildModel;

model CombinedTest "Test inner and outer in same model"
  inner World world(g=10.0);  // Override default g
  ChildModel child;
end CombinedTest;
