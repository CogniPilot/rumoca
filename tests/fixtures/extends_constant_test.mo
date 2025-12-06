// Test constant propagation with extends
block BaseBlock
  constant Real baseConst = 1.0;
  parameter Real baseParam = 2.0;
end BaseBlock;

block DerivedBlock
  extends BaseBlock;
  constant Real derivedConst = 3.0;
  parameter Real derivedParam = 4.0;
  Real x;
equation
  der(x) = baseConst + baseParam + derivedConst + derivedParam;
end DerivedBlock;

model ExtendsConstantTest
  DerivedBlock block1;
end ExtendsConstantTest;
