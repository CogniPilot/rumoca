// Test inner/outer component parsing
model World "Global coordinate system"
  parameter Real g = 9.81 "Gravitational acceleration";
end World;

model InnerOuterTest "Test model with inner/outer"
  World world; // Provides World instance to children
end InnerOuterTest;

model ChildModel "Model that uses outer reference"
  World world; // References World from enclosing scope
  Real v;
equation
  v = world.g;
end ChildModel;

model CombinedTest "Test inner and outer in same model"
  World world(g = 10.0); // Override default g
  ChildModel child;
end CombinedTest;
