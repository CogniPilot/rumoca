// Default quadrotor scene for rumoca lockstep run viewer.
//
// Scene script API:
//   ctx.onInit(api)  — called once after Three.js is ready
//   ctx.onFrame(api) — called each animation frame
//
// api object:
//   api.THREE        — Three.js library
//   api.scene        — THREE.Scene
//   api.state        — persistent object (store your meshes here)
//   api.get(name)    — read state variable (px, py, pz, q0-q3, omega_m1, etc.)
//   api.motors       — state object (has omega_m1..omega_m4)
//   api.camera       — THREE.Camera
//   api.cam          — { target: THREE.Vector3, dist, angle, elev } (mutable camera orbit)

ctx.onInit = function(api) {
  const THREE = api.THREE;
  const scene = api.scene;
  const s = api.state;

  // ── Desert sky ──────────────────────────────────────────────────
  const skyGeo = new THREE.SphereGeometry(200, 32, 16);
  const skyColors = [];
  const posAttr = skyGeo.getAttribute("position");
  for (let i = 0; i < posAttr.count; i++) {
    const y = posAttr.getY(i);
    const t = Math.max(0, Math.min(1, (y / 200 + 1) * 0.5));
    const r = 0.94 + (0.29 - 0.94) * Math.pow(t, 0.6);
    const g = 0.85 + (0.56 - 0.85) * Math.pow(t, 0.6);
    const b = 0.69 + (0.78 - 0.69) * Math.pow(t, 0.6);
    skyColors.push(r, g, b);
  }
  skyGeo.setAttribute("color", new THREE.Float32BufferAttribute(skyColors, 3));
  scene.add(new THREE.Mesh(skyGeo, new THREE.MeshBasicMaterial({ vertexColors: true, side: THREE.BackSide })));

  // ── Desert lighting ─────────────────────────────────────────────
  const sun = new THREE.DirectionalLight(0xfff0d0, 1.8);
  sun.position.set(8, 12, 4); scene.add(sun);
  const fill = new THREE.DirectionalLight(0xd4a060, 0.4);
  fill.position.set(-5, 3, -4); scene.add(fill);
  const rim = new THREE.DirectionalLight(0xffeebb, 0.25);
  rim.position.set(0, -1, -6); scene.add(rim);
  scene.add(new THREE.HemisphereLight(0x87ceeb, 0xc2956b, 0.5));

  // ── Desert ground ───────────────────────────────────────────────
  const sandMat = new THREE.MeshStandardMaterial({ color: 0xd4a860, roughness: 0.95, metalness: 0.02 });
  const floor = new THREE.Mesh(new THREE.PlaneGeometry(200, 200), sandMat);
  floor.rotation.x = -Math.PI / 2; floor.position.y = -0.01; scene.add(floor);
  const grid = new THREE.GridHelper(50, 50, 0xc49850, 0xc49850);
  grid.material.transparent = true; grid.material.opacity = 0.12;
  scene.add(grid);

  // ── Sand dunes ──────────────────────────────────────────────────
  const duneMat = new THREE.MeshStandardMaterial({ color: 0xd9b06a, roughness: 0.9 });
  const duneDarkMat = new THREE.MeshStandardMaterial({ color: 0xc49850, roughness: 0.95 });
  [
    { x:-18,z:20,sx:12,sy:1.5,sz:5,ry:0.3 }, { x:22,z:15,sx:15,sy:2.0,sz:6,ry:-0.2 },
    { x:-25,z:-10,sx:10,sy:1.2,sz:4,ry:0.5 }, { x:15,z:-22,sx:18,sy:2.5,sz:7,ry:0.1 },
    { x:-10,z:-25,sx:14,sy:1.8,sz:5,ry:-0.4 }, { x:30,z:-5,sx:8,sy:1.0,sz:4,ry:0.6 },
    { x:-30,z:5,sx:11,sy:1.3,sz:5,ry:-0.1 }, { x:0,z:30,sx:20,sy:2.2,sz:6,ry:0.15 },
  ].forEach((d) => {
    const dune = new THREE.Mesh(
      new THREE.SphereGeometry(1, 16, 8, 0, Math.PI*2, 0, Math.PI/2),
      Math.random() > 0.5 ? duneMat : duneDarkMat
    );
    dune.scale.set(d.sx, d.sy, d.sz);
    dune.position.set(d.x, -0.01, d.z); dune.rotation.y = d.ry;
    scene.add(dune);
  });

  // ── Desert rocks ────────────────────────────────────────────────
  const rockMat = new THREE.MeshStandardMaterial({ color: 0x8b7355, roughness: 0.85, metalness: 0.05 });
  const rockDarkMat = new THREE.MeshStandardMaterial({ color: 0x6b5740, roughness: 0.9, metalness: 0.05 });
  [
    {x:5,z:8,s:0.3},{x:-7,z:6,s:0.5},{x:9,z:-4,s:0.2},{x:-4,z:-8,s:0.4},
    {x:12,z:3,s:0.35},{x:-11,z:-3,s:0.25},{x:3,z:-10,s:0.45},{x:-8,z:10,s:0.3},
  ].forEach((r) => {
    const rock = new THREE.Mesh(
      new THREE.DodecahedronGeometry(r.s, 1),
      Math.random() > 0.5 ? rockMat : rockDarkMat
    );
    rock.position.set(r.x, r.s*0.3, r.z);
    rock.scale.set(1+Math.random()*0.5, 0.5+Math.random()*0.4, 1+Math.random()*0.3);
    rock.rotation.set(Math.random()*0.3, Math.random()*Math.PI, Math.random()*0.2);
    scene.add(rock);
  });

  // ── Cacti ───────────────────────────────────────────────────────
  const cactusMat = new THREE.MeshStandardMaterial({ color: 0x3a6b35, roughness: 0.8, metalness: 0.05 });
  [{x:-6,z:12},{x:14,z:7},{x:-13,z:-6},{x:8,z:-12},{x:-3,z:-15}].forEach((c) => {
    const h = 0.8 + Math.random() * 1.2;
    const trunk = new THREE.Mesh(new THREE.CylinderGeometry(0.12, 0.15, h, 8), cactusMat);
    trunk.position.set(c.x, h/2, c.z); scene.add(trunk);
    const top = new THREE.Mesh(new THREE.SphereGeometry(0.12, 8, 6), cactusMat);
    top.position.set(c.x, h, c.z); scene.add(top);
    if (Math.random() > 0.3) {
      const armH = 0.4 + Math.random()*0.4;
      const armY = h*0.4 + Math.random()*h*0.3;
      const dir = Math.random() > 0.5 ? 1 : -1;
      const aH = new THREE.Mesh(new THREE.CylinderGeometry(0.07, 0.08, 0.3, 6), cactusMat);
      aH.rotation.z = dir*Math.PI/2; aH.position.set(c.x+dir*0.2, armY, c.z); scene.add(aH);
      const aV = new THREE.Mesh(new THREE.CylinderGeometry(0.06, 0.07, armH, 6), cactusMat);
      aV.position.set(c.x+dir*0.35, armY+armH/2, c.z); scene.add(aV);
      const aT = new THREE.Mesh(new THREE.SphereGeometry(0.06, 6, 4), cactusMat);
      aT.position.set(c.x+dir*0.35, armY+armH, c.z); scene.add(aT);
    }
  });

  // ── High-fidelity quadrotor ─────────────────────────────────────
  const ARM_LEN = 0.28;
  const quad = new THREE.Group(); quad.name = "quadrotor";

  const matBody = new THREE.MeshStandardMaterial({ color: 0x2a2a2a, metalness: 0.4, roughness: 0.45 });
  const matBodyAccent = new THREE.MeshStandardMaterial({ color: 0x1a1a1e, metalness: 0.5, roughness: 0.3 });
  const matCarbon = new THREE.MeshStandardMaterial({ color: 0x222222, metalness: 0.3, roughness: 0.6 });
  const matMotor = new THREE.MeshStandardMaterial({ color: 0x111111, metalness: 0.85, roughness: 0.15 });
  const matMotorBell = new THREE.MeshStandardMaterial({ color: 0x333338, metalness: 0.9, roughness: 0.1 });
  const matLens = new THREE.MeshStandardMaterial({ color: 0x111122, metalness: 0.9, roughness: 0.05 });
  const matGimbal = new THREE.MeshStandardMaterial({ color: 0xaaaaaa, metalness: 0.7, roughness: 0.2 });
  const matBattery = new THREE.MeshStandardMaterial({ color: 0x2a2a30, metalness: 0.3, roughness: 0.6 });
  const matGuard = new THREE.MeshStandardMaterial({ color: 0x333333, metalness: 0.2, roughness: 0.7, transparent: true, opacity: 0.6 });
  const matSkid = new THREE.MeshStandardMaterial({ color: 0x444444, metalness: 0.3, roughness: 0.5 });

  // Central fuselage
  const bodyTop = new THREE.Mesh(
    new THREE.SphereGeometry(0.09, 24, 12, 0, Math.PI*2, 0, Math.PI/2), matBody
  );
  bodyTop.scale.set(1.3, 0.5, 1.0); bodyTop.position.y = 0.005; quad.add(bodyTop);
  const bodyBot = new THREE.Mesh(
    new THREE.SphereGeometry(0.09, 24, 12, 0, Math.PI*2, Math.PI/2, Math.PI/2), matBodyAccent
  );
  bodyBot.scale.set(1.3, 0.35, 1.0); bodyBot.position.y = 0.005; quad.add(bodyBot);
  for (const sz of [-1, 1]) {
    const strip = new THREE.Mesh(new THREE.BoxGeometry(0.22, 0.012, 0.004), matCarbon);
    strip.position.set(0, 0.005, sz*0.085); quad.add(strip);
  }
  const cover = new THREE.Mesh(new THREE.CylinderGeometry(0.065, 0.07, 0.008, 20), matBodyAccent);
  cover.position.y = 0.05; quad.add(cover);

  // GPS / antenna mast
  const antBase = new THREE.Mesh(new THREE.CylinderGeometry(0.012, 0.014, 0.015, 8), matCarbon);
  antBase.position.set(0, 0.058, -0.02); quad.add(antBase);
  const antMast = new THREE.Mesh(new THREE.CylinderGeometry(0.003, 0.003, 0.035, 6), matGimbal);
  antMast.position.set(0, 0.082, -0.02); quad.add(antMast);
  const antTop = new THREE.Mesh(new THREE.SphereGeometry(0.006, 8, 6), matGimbal);
  antTop.position.set(0, 0.1, -0.02); quad.add(antTop);

  // Battery pack
  const batt = new THREE.Mesh(new THREE.BoxGeometry(0.08, 0.022, 0.045), matBattery);
  batt.position.set(0, 0.04, 0.01); quad.add(batt);
  const battStripe = new THREE.Mesh(
    new THREE.BoxGeometry(0.04, 0.023, 0.002),
    new THREE.MeshStandardMaterial({ color: 0xddaa00, metalness: 0.3, roughness: 0.5 })
  );
  battStripe.position.set(0, 0.04, 0.034); quad.add(battStripe);

  // Camera gimbal
  const gimbalMount = new THREE.Mesh(new THREE.BoxGeometry(0.03, 0.012, 0.03), matGimbal);
  gimbalMount.position.set(0, -0.025, 0.065); quad.add(gimbalMount);
  const gimbalYoke = new THREE.Mesh(new THREE.BoxGeometry(0.025, 0.02, 0.025), matGimbal);
  gimbalYoke.position.set(0, -0.038, 0.065); quad.add(gimbalYoke);
  const camHousing = new THREE.Mesh(new THREE.BoxGeometry(0.028, 0.018, 0.022), matBody);
  camHousing.position.set(0, -0.038, 0.065); quad.add(camHousing);
  const lens = new THREE.Mesh(new THREE.CylinderGeometry(0.007, 0.008, 0.008, 12), matLens);
  lens.rotation.x = Math.PI/2; lens.position.set(0, -0.038, 0.078); quad.add(lens);
  const lensGlass = new THREE.Mesh(
    new THREE.CircleGeometry(0.006, 12),
    new THREE.MeshStandardMaterial({ color: 0x223355, metalness: 1.0, roughness: 0.0 })
  );
  lensGlass.position.set(0, -0.038, 0.0825); quad.add(lensGlass);

  // Arms + motors + propellers
  const armConfigs = [
    { angle: Math.PI/4,      cw: false, ledColor: 0x00ff44 },
    { angle: -Math.PI/4,     cw: true,  ledColor: 0xff2200 },
    { angle: 3*Math.PI/4,    cw: true,  ledColor: 0xffffff },
    { angle: -3*Math.PI/4,   cw: false, ledColor: 0xffffff },
  ];
  s.propGroups = [];

  function createPropBlade(cw) {
    const blade = new THREE.Group();
    const bladeLen = 0.11, segments = 6;
    for (let i = 0; i < segments; i++) {
      const t = (i + 0.5) / segments;
      const segLen = bladeLen / segments;
      const chord = 0.022 * (1.0 - 0.5*t);
      const thickness = 0.003 * (1.0 - 0.6*t);
      const seg = new THREE.Mesh(
        new THREE.BoxGeometry(segLen, thickness, chord),
        new THREE.MeshStandardMaterial({ color: 0x1a1a1a, metalness: 0.15, roughness: 0.55, side: THREE.DoubleSide })
      );
      const r = 0.012 + bladeLen * t;
      seg.position.x = (cw ? 1 : -1) * r;
      seg.rotation.x = (cw ? 1 : -1) * (0.35 - 0.25*t);
      seg.position.y = -t*t*0.005;
      blade.add(seg);
    }
    const tipCap = new THREE.Mesh(
      new THREE.SphereGeometry(0.006, 6, 4),
      new THREE.MeshStandardMaterial({ color: 0x1a1a1a, roughness: 0.5 })
    );
    tipCap.scale.set(0.5, 0.3, 1.0);
    tipCap.position.x = (cw ? 1 : -1) * (0.012 + bladeLen);
    blade.add(tipCap);
    return blade;
  }

  armConfigs.forEach((cfg) => {
    const dx = Math.sin(cfg.angle) * ARM_LEN;
    const dz = Math.cos(cfg.angle) * ARM_LEN;

    const arm = new THREE.Mesh(new THREE.CylinderGeometry(0.009, 0.012, ARM_LEN, 8), matCarbon);
    arm.rotation.z = Math.PI/2; arm.rotation.y = -cfg.angle + Math.PI/2;
    arm.position.set(dx/2, 0.005, dz/2); quad.add(arm);

    const rib = new THREE.Mesh(new THREE.BoxGeometry(ARM_LEN*0.7, 0.018, 0.004), matCarbon);
    rib.rotation.y = -cfg.angle + Math.PI/2;
    rib.position.set(dx*0.55, 0.005, dz*0.55); quad.add(rib);

    const motorBase = new THREE.Mesh(new THREE.CylinderGeometry(0.022, 0.024, 0.008, 12), matCarbon);
    motorBase.position.set(dx, 0.0, dz); quad.add(motorBase);
    const stator = new THREE.Mesh(new THREE.CylinderGeometry(0.014, 0.014, 0.018, 10), matMotor);
    stator.position.set(dx, 0.013, dz); quad.add(stator);
    const bellTop = new THREE.Mesh(new THREE.CylinderGeometry(0.018, 0.020, 0.012, 12), matMotorBell);
    bellTop.position.set(dx, 0.024, dz); quad.add(bellTop);
    const bellRing = new THREE.Mesh(new THREE.TorusGeometry(0.019, 0.002, 6, 16), matMotorBell);
    bellRing.rotation.x = Math.PI/2; bellRing.position.set(dx, 0.018, dz); quad.add(bellRing);
    const shaft = new THREE.Mesh(new THREE.CylinderGeometry(0.003, 0.003, 0.01, 6), matMotor);
    shaft.position.set(dx, 0.035, dz); quad.add(shaft);

    const propGroup = new THREE.Group();
    propGroup.position.set(dx, 0.038, dz);
    const hub = new THREE.Mesh(new THREE.CylinderGeometry(0.008, 0.008, 0.005, 8), matCarbon);
    propGroup.add(hub);
    for (let b = 0; b < 2; b++) {
      const blade = createPropBlade(cfg.cw);
      blade.rotation.y = b * Math.PI;
      propGroup.add(blade);
    }
    quad.add(propGroup);
    s.propGroups.push({ group: propGroup, cw: cfg.cw });

    const guard = new THREE.Mesh(new THREE.TorusGeometry(0.13, 0.004, 6, 32), matGuard);
    guard.rotation.x = Math.PI/2; guard.position.set(dx, 0.032, dz); quad.add(guard);
    for (const strutAngle of [cfg.angle - 0.3, cfg.angle + 0.3]) {
      const sx = Math.sin(strutAngle)*0.12, sz = Math.cos(strutAngle)*0.12;
      const strut = new THREE.Mesh(new THREE.CylinderGeometry(0.002, 0.002, 0.04, 4), matCarbon);
      strut.position.set(dx+sx*0.3, 0.025, dz+sz*0.3);
      strut.rotation.z = (sx > 0 ? -1 : 1)*0.4;
      strut.rotation.x = (sz > 0 ? 1 : -1)*0.4;
      quad.add(strut);
    }

    const led = new THREE.Mesh(
      new THREE.SphereGeometry(0.005, 8, 6),
      new THREE.MeshStandardMaterial({ color: cfg.ledColor, emissive: cfg.ledColor, emissiveIntensity: 2.0 })
    );
    led.position.set(dx*0.85, -0.015, dz*0.85); quad.add(led);
    const ledLight = new THREE.PointLight(cfg.ledColor, 0.15, 0.3);
    ledLight.position.copy(led.position); quad.add(ledLight);
  });

  // Landing gear
  for (const side of [-1, 1]) {
    const skidZ = side * 0.055;
    const rail = new THREE.Mesh(new THREE.CapsuleGeometry(0.005, 0.22, 4, 8), matSkid);
    rail.rotation.z = Math.PI/2; rail.position.set(0, -0.065, skidZ); quad.add(rail);
    for (const fb of [-0.06, 0.06]) {
      const strut = new THREE.Mesh(new THREE.CylinderGeometry(0.004, 0.005, 0.045, 6), matSkid);
      strut.position.set(fb, -0.042, skidZ); strut.rotation.z = side*0.08; quad.add(strut);
    }
    const brace = new THREE.Mesh(new THREE.CylinderGeometry(0.003, 0.003, 0.12, 4), matSkid);
    brace.rotation.z = Math.PI/2; brace.position.set(0, -0.048, skidZ); quad.add(brace);
    for (const xOff of [-0.11, 0.11]) {
      const foot = new THREE.Mesh(
        new THREE.SphereGeometry(0.006, 6, 4),
        new THREE.MeshStandardMaterial({ color: 0x111111, roughness: 0.9 })
      );
      foot.position.set(xOff, -0.068, skidZ); foot.scale.y = 0.5; quad.add(foot);
    }
  }

  // Rear status LED bar
  const ledBar = new THREE.Mesh(
    new THREE.BoxGeometry(0.06, 0.006, 0.004),
    new THREE.MeshStandardMaterial({ color: 0x00aaff, emissive: 0x0066ff, emissiveIntensity: 1.5 })
  );
  ledBar.position.set(0, 0.01, -0.09); quad.add(ledBar);

  // Front headlight
  const headlight = new THREE.Mesh(
    new THREE.SphereGeometry(0.004, 6, 4),
    new THREE.MeshStandardMaterial({ color: 0xffffee, emissive: 0xffffcc, emissiveIntensity: 3.0 })
  );
  headlight.position.set(0, -0.01, 0.092); quad.add(headlight);
  const headlightBeam = new THREE.SpotLight(0xffffdd, 0.4, 2, 0.5, 0.8);
  headlightBeam.position.set(0, -0.01, 0.092);
  headlightBeam.target.position.set(0, -0.5, 0.5);
  quad.add(headlightBeam); quad.add(headlightBeam.target);

  scene.add(quad);
  s.quad = quad;

  // ── Flight trail ────────────────────────────────────────────────
  const maxTrail = 800;
  s.trailPos = new Float32Array(maxTrail * 3);
  s.trailGeo = new THREE.BufferGeometry();
  s.trailGeo.setAttribute("position", new THREE.BufferAttribute(s.trailPos, 3));
  s.trailGeo.setDrawRange(0, 0);
  const trail = new THREE.Line(s.trailGeo,
    new THREE.LineBasicMaterial({ color: 0x1155cc, transparent: true, opacity: 0.6 })
  );
  scene.add(trail);
  s.trailCount = 0;
  s.maxTrail = maxTrail;

  // ── Ground shadow ───────────────────────────────────────────────
  s.shadow = new THREE.Mesh(
    new THREE.CircleGeometry(0.2, 20),
    new THREE.MeshBasicMaterial({ color: 0x3a2a10, transparent: true, opacity: 0.3 })
  );
  s.shadow.rotation.x = -Math.PI / 2;
  s.shadow.position.y = 0.003;
  scene.add(s.shadow);
};

ctx.onFrame = function(api) {
  const THREE = api.THREE;
  const s = api.state;
  const get = api.get;
  const motors = api.motors;

  const px = get("px") ?? 0;
  const py = get("py") ?? 0;
  const pz = get("pz") ?? 0;
  const q0 = get("q0") ?? 1;
  const q1 = get("q1") ?? 0;
  const q2 = get("q2") ?? 0;
  const q3 = get("q3") ?? 0;

  // NED to Three.js: x_three = py, y_three = -pz, z_three = px
  const GEAR_HEIGHT = 0.068;
  const tx = py, ty = -pz + GEAR_HEIGHT, tz = px;

  // Build DCM from quaternion (body-to-world, NED)
  const R11 = 1-2*(q2*q2+q3*q3), R12 = 2*(q1*q2-q0*q3), R13 = 2*(q1*q3+q0*q2);
  const R21 = 2*(q1*q2+q0*q3), R22 = 1-2*(q1*q1+q3*q3), R23 = 2*(q2*q3-q0*q1);
  const R31 = 2*(q1*q3-q0*q2), R32 = 2*(q2*q3+q0*q1), R33 = 1-2*(q1*q1+q2*q2);

  // Transform: M_three = T * R_ned * T^(-1)
  const m = s.quad.matrix, e = m.elements;
  e[0]=R22;  e[1]=-R32; e[2]=R12;  e[3]=0;
  e[4]=-R23; e[5]=R33;  e[6]=-R13; e[7]=0;
  e[8]=R21;  e[9]=-R31; e[10]=R11; e[11]=0;
  e[12]=tx;  e[13]=ty;  e[14]=tz;  e[15]=1;
  s.quad.matrixAutoUpdate = false;
  s.quad.matrixWorldNeedsUpdate = true;

  // Spin propellers (individual per motor, matching working code)
  const omegas = [
    motors.omega_m1 ?? 0, motors.omega_m2 ?? 0,
    motors.omega_m3 ?? 0, motors.omega_m4 ?? 0,
  ];
  s.propGroups.forEach((p, i) => {
    const speed = Math.min(omegas[i] / 500, 2.0) * 0.5;
    p.group.rotation.y += (p.cw ? 1 : -1) * (0.3 + speed);
  });

  // Trail
  const idx = s.trailCount % s.maxTrail;
  s.trailPos[idx * 3] = tx;
  s.trailPos[idx * 3 + 1] = ty;
  s.trailPos[idx * 3 + 2] = tz;
  s.trailCount++;
  s.trailGeo.attributes.position.needsUpdate = true;
  s.trailGeo.setDrawRange(0, Math.min(s.trailCount, s.maxTrail));

  // Shadow
  s.shadow.position.set(tx, 0.003, tz);
  const alt = Math.max(ty, 0.01);
  const sc = Math.max(0.4, 1.2 - alt * 0.04);
  s.shadow.scale.set(sc, sc, 1);
  s.shadow.material.opacity = Math.max(0.03, 0.25 - alt * 0.015);

  // Camera follow
  const c = api.cam;
  c.target.lerp(new THREE.Vector3(tx, ty, tz), 0.05);
  api.camera.position.set(
    c.target.x + c.dist * Math.sin(c.angle) * Math.cos(c.elev),
    c.target.y + c.dist * Math.sin(c.elev),
    c.target.z + c.dist * Math.cos(c.angle) * Math.cos(c.elev)
  );
  api.camera.lookAt(c.target);
};
