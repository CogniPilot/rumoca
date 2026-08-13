within;
connector Pin
  Real v;
  flow Real i;
end Pin;
model SliceMember
  Pin pin[3];
  Real v[3] = pin[:].v;
  Real x(start = 0, fixed = true);
equation
  for k in 1:3 loop
    pin[k].v = k * 10.0;
  end for;
  der(x) = v[2];
end SliceMember;
