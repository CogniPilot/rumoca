// Rover scene for the rumoca sim viewer.
//
// Matches the dessert environment used by the quadrotor demo (sky, sun,
// dunes, rocks, cacti) so a rover and a drone can share the same world.
//
// Expects these state keys from rumoca-scenario.toml `[signals.viewer]`:
//   x, y             — planar world position [m]
//   theta            — heading [rad] (0 = +x axis)
//   wheel_rpm        — rear-wheel angular velocity [rad/s] (rolling)
//   front_wheel_yaw  — front steering angle [rad]

// Procedural rover geometry keeps the demo self-contained and offline.

ctx.onInit = async function(api) {
  const THREE = api.THREE;
  const scene = api.scene;
  const s = api.state;

  // ── Desert sky (matches quadrotor_scene.js) ───────────────────────
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

  // ── Desert lighting ───────────────────────────────────────────────
  const sun = new THREE.DirectionalLight(0xfff0d0, 1.8);
  sun.position.set(8, 12, 4); scene.add(sun);
  const fill = new THREE.DirectionalLight(0xd4a060, 0.4);
  fill.position.set(-5, 3, -4); scene.add(fill);
  const rim = new THREE.DirectionalLight(0xffeebb, 0.25);
  rim.position.set(0, -1, -6); scene.add(rim);
  scene.add(new THREE.HemisphereLight(0x87ceeb, 0xc2956b, 0.5));

  // ── Desert ground ─────────────────────────────────────────────────
  const sandMat = new THREE.MeshStandardMaterial({ color: 0xd4a860, roughness: 0.95, metalness: 0.02 });
  const floor = new THREE.Mesh(new THREE.PlaneGeometry(400, 400), sandMat);
  floor.rotation.x = -Math.PI / 2; floor.position.y = -0.01; scene.add(floor);
  const grid = new THREE.GridHelper(50, 50, 0xc49850, 0xc49850);
  grid.material.transparent = true; grid.material.opacity = 0.12;
  scene.add(grid);

  // ── Sand dunes ────────────────────────────────────────────────────
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

  // ── Desert rocks ──────────────────────────────────────────────────
  const rockMat = new THREE.MeshStandardMaterial({ color: 0x8b7355, roughness: 0.85, metalness: 0.05 });
  const rockDarkMat = new THREE.MeshStandardMaterial({ color: 0x6b5740, roughness: 0.9, metalness: 0.05 });
  [
    {x:5,z:8,sc:0.3},{x:-7,z:6,sc:0.5},{x:9,z:-4,sc:0.2},{x:-4,z:-8,sc:0.4},
    {x:12,z:3,sc:0.35},{x:-11,z:-3,sc:0.25},{x:3,z:-10,sc:0.45},{x:-8,z:10,sc:0.3},
  ].forEach((r) => {
    const rock = new THREE.Mesh(
      new THREE.DodecahedronGeometry(r.sc, 1),
      Math.random() > 0.5 ? rockMat : rockDarkMat
    );
    rock.position.set(r.x, r.sc*0.3, r.z);
    rock.scale.set(1+Math.random()*0.5, 0.5+Math.random()*0.4, 1+Math.random()*0.3);
    rock.rotation.set(Math.random()*0.3, Math.random()*Math.PI, Math.random()*0.2);
    scene.add(rock);
  });

  // ── Cacti ─────────────────────────────────────────────────────────
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

  // ── Rover (GLB jeep) ──────────────────────────────────────────────
  const rover = new THREE.Group(); rover.name = "rover";
  scene.add(rover);

  // Ground shadow blob — size approximates Rover.mo wheelbase.
  s.shadow = new THREE.Mesh(
    new THREE.CircleGeometry(0.4, 24),
    new THREE.MeshBasicMaterial({ color: 0x000000, transparent: true, opacity: 0.25 })
  );
  s.shadow.rotation.x = -Math.PI / 2;
  s.shadow.position.y = 0.001;
  scene.add(s.shadow);

  const wheelPivots = {};
  const chassisMat = new THREE.MeshStandardMaterial({ color: 0xd85b2a, roughness: 0.58, metalness: 0.08 });
  const deckMat = new THREE.MeshStandardMaterial({ color: 0x2a2f35, roughness: 0.65, metalness: 0.15 });
  const wheelMat = new THREE.MeshStandardMaterial({ color: 0x181818, roughness: 0.9, metalness: 0.02 });
  const hubMat = new THREE.MeshStandardMaterial({ color: 0xb6b6b6, roughness: 0.35, metalness: 0.4 });

  const body = new THREE.Mesh(new THREE.BoxGeometry(0.72, 0.18, 1.0), chassisMat);
  body.position.y = 0.32;
  rover.add(body);

  const cabin = new THREE.Mesh(new THREE.BoxGeometry(0.42, 0.22, 0.34), deckMat);
  cabin.position.set(0, 0.53, -0.08);
  rover.add(cabin);

  const bumper = new THREE.Mesh(new THREE.BoxGeometry(0.62, 0.08, 0.08), deckMat);
  bumper.position.set(0, 0.25, 0.56);
  rover.add(bumper);

  function addWheel(slot, x, z) {
    const pivot = new THREE.Group();
    pivot.position.set(x, 0.22, z);
    const wheel = new THREE.Mesh(new THREE.CylinderGeometry(0.16, 0.16, 0.12, 24), wheelMat);
    wheel.rotation.z = Math.PI / 2;
    const hub = new THREE.Mesh(new THREE.CylinderGeometry(0.07, 0.07, 0.13, 16), hubMat);
    hub.rotation.z = Math.PI / 2;
    pivot.add(wheel);
    pivot.add(hub);
    rover.add(pivot);
    wheelPivots[slot] = { pivot, wheel };
  }

  addWheel("frontLeft", -0.42, 0.34);
  addWheel("frontRight", 0.42, 0.34);
  addWheel("rearLeft", -0.42, -0.36);
  addWheel("rearRight", 0.42, -0.36);

  // Trail
  s.maxTrail = 512;
  s.trailPos = new Float32Array(s.maxTrail * 3);
  s.trailCount = 0;
  s.trailGeo = new THREE.BufferGeometry();
  s.trailGeo.setAttribute("position", new THREE.BufferAttribute(s.trailPos, 3));
  s.trailMat = new THREE.LineBasicMaterial({ color: 0xffaa55, transparent: true, opacity: 0.5 });
  s.trail = new THREE.Line(s.trailGeo, s.trailMat);
  scene.add(s.trail);

  s.rover = rover;
  s.wheels = wheelPivots;

  // Rolling-phase integration (wall-time) from wheel_rpm.
  s.rollPhase = 0;
  s.lastWall = null;

  if (api.cam) {
    api.cam.dist = 3.5;
    api.cam.angle = Math.PI; // chase camera behind rover
    api.cam.elev = 0.4;
  }
};

ctx.onFrame = function(api) {
  const THREE = api.THREE;
  const s = api.state;
  const get = api.get;

  // onInit is async (GLB load); the viewer calls onFrame without awaiting,
  // so early-return until the model is ready.
  if (!s.rover || !s.wheels) return;

  const rpm   = get("wheel_rpm") ?? 0;
  const steer = get("front_wheel_yaw") ?? 0;

  // The chassis pose comes from the viewer's [[viewer.frame]] "chassis"
  // (planar x/y + heading in model FLU coordinates); the viewer owns the
  // FLU-to-renderer conversion, so the configured onboard camera and this
  // placement can never drift apart.
  const chassis = api.frames && api.frames.get("chassis");
  if (!chassis) return;
  s.rover.matrixAutoUpdate = false;
  s.rover.matrix.copy(chassis);
  s.rover.matrixWorldNeedsUpdate = true;
  if (!s.chassisPos) s.chassisPos = new THREE.Vector3();
  const pos = s.chassisPos.setFromMatrixPosition(chassis);
  s.shadow.position.set(pos.x, 0.001, pos.z);

  // Integrate rolling phase from wheel_rpm (wall-time so it's smooth
  // regardless of sim realtime ratio).
  const nowMs = (typeof performance !== "undefined") ? performance.now() : Date.now();
  const dtWall = s.lastWall == null ? 0 : Math.min(0.1, (nowMs - s.lastWall) / 1000);
  s.lastWall = nowMs;
  s.rollPhase += rpm * dtWall;
  // Wheels' axle is along their local Z (smallest bbox dimension from the
  // GLB analysis), so rolling is rotation about Z.
  s.wheels.rearLeft.wheel.rotation.z   = s.rollPhase;
  s.wheels.rearRight.wheel.rotation.z  = s.rollPhase;
  s.wheels.frontLeft.wheel.rotation.z  = s.rollPhase;
  s.wheels.frontRight.wheel.rotation.z = s.rollPhase;

  // Front pivots yaw with steering angle. Model CCW yaw (about FLU +z) maps
  // to renderer rotation.y = -angle under the viewer frame convention.
  s.wheels.frontLeft.pivot.rotation.y  = -steer;
  s.wheels.frontRight.pivot.rotation.y = -steer;

  // Trail
  const idx = s.trailCount % s.maxTrail;
  s.trailPos[idx*3    ] = pos.x;
  s.trailPos[idx*3 + 1] = 0.02;
  s.trailPos[idx*3 + 2] = pos.z;
  s.trailCount++;
  s.trailGeo.attributes.position.needsUpdate = true;
  s.trailGeo.setDrawRange(0, Math.min(s.trailCount, s.maxTrail));

  // Chase camera (scene-controlled mode)
  if (api.cameraMode === "scene" && api.cam && api.camera) {
    const c = api.cam;
    c.target.lerp(new THREE.Vector3(pos.x, 0.15, pos.z), 0.08);
    api.camera.position.set(
      c.target.x + c.dist * Math.sin(c.angle) * Math.cos(c.elev),
      c.target.y + c.dist * Math.sin(c.elev),
      c.target.z + c.dist * Math.cos(c.angle) * Math.cos(c.elev)
    );
    api.camera.lookAt(c.target);
  }
};
