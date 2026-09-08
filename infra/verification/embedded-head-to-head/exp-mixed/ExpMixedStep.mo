package ExpMixedPkg
  function wedge
    input Real v[3];
    output Real S[3,3];
  algorithm
    S[1,1] := 0; S[1,2] := -v[3]; S[1,3] := v[2];
    S[2,1] := v[3]; S[2,2] := 0; S[2,3] := -v[1];
    S[3,1] := -v[2]; S[3,2] := v[1]; S[3,3] := 0;
  end wedge;

  function exp_map
    input Real v[3];
    output Real q[4];
  protected
    Real theta_sq;
    Real half_theta;
    Real A;
    Real B;
    constant Real eps = 1e-8;
  algorithm
    theta_sq := v[1]^2 + v[2]^2 + v[3]^2;
    if theta_sq < eps then
      B := 1.0 - theta_sq / 8.0;
      A := 0.5 - theta_sq / 48.0;
    else
      half_theta := sqrt(theta_sq) / 2.0;
      B := cos(half_theta);
      A := sin(half_theta) / sqrt(theta_sq);
    end if;
    q[1] := B;
    q[2] := A * v[1];
    q[3] := A * v[2];
    q[4] := A * v[3];
  end exp_map;

  function product
    input Real q[4];
    input Real p[4];
    output Real r[4];
  algorithm
    r[1] := q[1]*p[1] - q[2]*p[2] - q[3]*p[3] - q[4]*p[4];
    r[2] := q[2]*p[1] + q[1]*p[2] - q[4]*p[3] + q[3]*p[4];
    r[3] := q[3]*p[1] + q[4]*p[2] + q[1]*p[3] - q[2]*p[4];
    r[4] := q[4]*p[1] - q[3]*p[2] + q[2]*p[3] + q[1]*p[4];
  end product;

  function to_DCM
    input Real q[4];
    output Real R[3,3];
  protected
    Real a, b, c, d;
    Real aa, ab, ac, ad, bb, bc, bd, cc, cd, dd;
  algorithm
    a := q[1]; b := q[2]; c := q[3]; d := q[4];
    aa := a*a; ab := a*b; ac := a*c; ad := a*d;
    bb := b*b; bc := b*c; bd := b*d;
    cc := c*c; cd := c*d; dd := d*d;
    R[1,1] := aa + bb - cc - dd;
    R[1,2] := 2*(bc - ad);
    R[1,3] := 2*(bd + ac);
    R[2,1] := 2*(bc + ad);
    R[2,2] := aa - bb + cc - dd;
    R[2,3] := 2*(cd - ab);
    R[3,1] := 2*(bd - ac);
    R[3,2] := 2*(cd + ab);
    R[3,3] := aa - bb - cc + dd;
  end to_DCM;

  function exp_mixed
    input Real X0[10];
    input Real l[9];
    input Real r[9];
    input Real B[2,2];
    output Real X1[10];
  protected
    Real omega_l[3], omega_r[3];
    Real theta_sq;
    Real C1, C2, C3;
    Real theta;
    constant Real eps = 1e-2;
    Real Om[3,3], Om2[3,3];
    Real Al[3,2], Ar[3,2];
    Real Nl[3,2], Nr[3,2];
    Real I2[2,2], IpB[2,2];
    Real q_l[4], q_r[4], q_r0[4], q1[4];
    Real R_r0[3,3], R_r[3,3];
    Real P0[3,2], P1[3,2];
    Real term1[3,2], term2[3,2], term3[3,2];
  algorithm
    omega_l := l[7:9];
    omega_r := r[7:9];
    theta_sq := omega_l[1]^2 + omega_l[2]^2 + omega_l[3]^2;
    if theta_sq < eps then
      C1 := 0.5 - theta_sq / 24.0;
      C2 := 1.0/6.0 - theta_sq / 120.0;
      C3 := 1.0/24.0 - theta_sq / 720.0;
    else
      theta := sqrt(theta_sq);
      C1 := (1.0 - cos(theta)) / theta_sq;
      C2 := (theta - sin(theta)) / (theta_sq * theta);
      C3 := (theta_sq/2.0 + cos(theta) - 1.0) / (theta_sq * theta_sq);
    end if;
    Om := wedge(omega_l);
    Om2 := Om * Om;
    Al := {{l[4], l[1]}, {l[5], l[2]}, {l[6], l[3]}};
    Ar := {{r[4], r[1]}, {r[5], r[2]}, {r[6], r[3]}};
    I2 := identity(2);
    Nl := Al + 0.5 * Al * B
      + Om * Al * (C1 * I2 + C2 * B)
      + Om2 * Al * (C2 * I2 + C3 * B);

    theta_sq := omega_r[1]^2 + omega_r[2]^2 + omega_r[3]^2;
    if theta_sq < eps then
      C1 := 0.5 - theta_sq / 24.0;
      C2 := 1.0/6.0 - theta_sq / 120.0;
      C3 := 1.0/24.0 - theta_sq / 720.0;
    else
      theta := sqrt(theta_sq);
      C1 := (1.0 - cos(theta)) / theta_sq;
      C2 := (theta - sin(theta)) / (theta_sq * theta);
      C3 := (theta_sq/2.0 + cos(theta) - 1.0) / (theta_sq * theta_sq);
    end if;
    Om := wedge(omega_r);
    Om2 := Om * Om;
    Nr := Ar - 0.5 * Ar * B
      + Om * Ar * (C1 * I2 - C2 * B)
      + Om2 * Ar * (C2 * I2 - C3 * B);
    q_l := exp_map(omega_l);
    q_r := exp_map(omega_r);
    q_r0 := product(q_r, X0[7:10]);
    q1 := product(q_r0, q_l);
    R_r0 := to_DCM(q_r0);
    R_r := to_DCM(q_r);
    P0 := {{X0[4], X0[1]}, {X0[5], X0[2]}, {X0[6], X0[3]}};
    IpB := I2 + B;
    term1 := R_r0 * Nl;
    term2 := R_r * P0 + Nr;
    term3 := term2 * IpB;
    P1 := term1 + term3;
    X1[1] := P1[1,2]; X1[2] := P1[2,2]; X1[3] := P1[3,2];
    X1[4] := P1[1,1]; X1[5] := P1[2,1]; X1[6] := P1[3,1];
    X1[7] := q1[1]; X1[8] := q1[2]; X1[9] := q1[3]; X1[10] := q1[4];
  end exp_mixed;
end ExpMixedPkg;

model ExpMixedStep
  constant Real samplePeriod = 0.01;
  input Real X0[10];
  input Real l[9];
  input Real r[9];
  input Real B[2,2];
  discrete output Real X1[10](each start = 0.0);
equation
  when sample(0.0, samplePeriod) then
    X1 = ExpMixedPkg.exp_mixed(X0, l, r, B);
  end when;
end ExpMixedStep;
