model Fourbar1ConnectOrderA
  inner Modelica.Mechanics.MultiBody.World world;
  Modelica.Mechanics.MultiBody.Joints.Revolute j1(n={1,0,0}, stateSelect=StateSelect.always, phi(fixed=true), w(start=5.235987755982989, fixed=true));
  Modelica.Mechanics.MultiBody.Joints.Prismatic j2(n={1,0,0}, s(start=-0.2), boxWidth=0.05);
  Modelica.Mechanics.MultiBody.Parts.BodyCylinder b1(r={0,0.5,0.1}, diameter=0.05);
  Modelica.Mechanics.MultiBody.Parts.BodyCylinder b2(r={0,0.2,0}, diameter=0.05);
  Modelica.Mechanics.MultiBody.Parts.BodyCylinder b3(r={-1,0.3,0.1}, diameter=0.05);
  Modelica.Mechanics.MultiBody.Joints.Revolute rev(n={0,1,0});
  Modelica.Mechanics.MultiBody.Joints.Revolute rev1;
  Modelica.Mechanics.MultiBody.Joints.Revolute j3(n={1,0,0});
  Modelica.Mechanics.MultiBody.Joints.Revolute j4(n={0,1,0});
  Modelica.Mechanics.MultiBody.Joints.Revolute j5(n={0,0,1});
  Modelica.Mechanics.MultiBody.Parts.FixedTranslation b0(animation=false, r={1.2,0,0});
equation
  connect(b0.frame_a, world.frame_b);
  connect(b0.frame_b, j2.frame_a);
  connect(j2.frame_b, b2.frame_a);
  connect(rev.frame_a, b2.frame_b);
  connect(rev.frame_b, rev1.frame_a);
  connect(rev1.frame_b, b3.frame_a);
  connect(j5.frame_b, b3.frame_b);
  connect(j4.frame_b, j5.frame_a);
  connect(j3.frame_b, j4.frame_a);
  connect(world.frame_b, j1.frame_a);
  connect(j1.frame_b, b1.frame_a);
  connect(b1.frame_b, j3.frame_a);
end Fourbar1ConnectOrderA;
