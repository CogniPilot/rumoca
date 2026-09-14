model TensorFixedAnchor
  parameter Real A[3,3] = {{2,1,0},{1,3,1},{0,1,2}};
  Real anchor[3];
  Real anchor_v[3];
  Real position[3](each stateSelect=StateSelect.avoid);
  Real velocity[3];
  Real acceleration[3];
  Real relative[3];
equation
  anchor = {1,2,3};
  anchor_v = der(anchor);
  relative = A*(position - anchor);
  relative[1] = 0;
  relative[2] = 0;
  relative[3] = 0;
  der(position) = velocity;
  der(velocity) = acceleration;
end TensorFixedAnchor;
