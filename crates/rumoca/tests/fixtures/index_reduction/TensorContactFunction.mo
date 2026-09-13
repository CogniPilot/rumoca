model TensorContact
  function orientedAxis
    input Integer axis;
    input Real angle;
    output Real v[3];
  algorithm
    v := if axis == 1 then {0,cos(angle),sin(angle)}
      elseif axis == 2 then {cos(angle),0,sin(angle)}
      else {cos(angle),sin(angle),0};
  end orientedAxis;
  parameter Real radius = 1;
  parameter Real theta0 = 0.2;
  Real theta(start=theta0, fixed=true, stateSelect=StateSelect.always);
  Real z(start=radius*cos(theta0));
  Real vz(start=-radius*sin(theta0));
  Real force;
  Real normal[3];
  Real axis[3];
  Real auxiliary[3];
  Real longitudinal[3];
  Real road[3];
  Real delta[3];
  Real s;
  Real w;
equation
  der(theta) = 1;
  der(z) = vz;
  der(vz) = force;
  normal = {0,0,1};
  axis = orientedAxis(1,theta);
  auxiliary = cross(normal, axis);
  longitudinal = auxiliary/sqrt(auxiliary*auxiliary);
  road = {s,w,0};
  delta = road - {0,0,z};
  0 = delta*axis;
  0 = delta*longitudinal;
  radius = delta*cross(longitudinal,axis);
  assert(abs(axis[3]) < 0.99, "Contact basis is singular");
end TensorContact;
