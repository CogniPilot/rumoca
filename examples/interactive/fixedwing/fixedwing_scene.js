// Fixed-wing scene for the rumoca sim viewer — desert environment (shared with
// the quadrotor example) plus the rigged Airplane.glb model whose control
// surfaces are driven from the controller outputs.
//
// Scene script API:
//   ctx.onInit(api)  — called once after Three.js is ready (may be async)
//   ctx.onFrame(api) — called each animation frame
//   api: { THREE, scene, renderer, state, get(name), camera, cam, GLTFLoader }
//
// Conventions: world Z-up, body FLU, quat {w,x,y,z} body->world. The Airplane
// GLB is authored nose +Z, up +Y, LEFT wing +X, so the placement matrix uses
// M = A'*R*A'^-1 with A' mapping model-FLU {fwd,left,up} -> three {left,up,fwd}.

function numericSignal(value) {
  const n = Number(value);
  return Number.isFinite(n) ? n : 0;
}

function deterministicUnit(seed) {
  const x = Math.sin(seed * 12.9898) * 43758.5453;
  return x - Math.floor(x);
}

function createCloudTexture(THREE) {
  const c = document.createElement("canvas");
  c.width = 256; c.height = 128;
  const g = c.getContext("2d");
  g.clearRect(0, 0, 256, 128);
  for (const lobe of [
    { x: 80, y: 70, r: 46 }, { x: 120, y: 54, r: 54 },
    { x: 165, y: 62, r: 50 }, { x: 130, y: 80, r: 64 },
  ]) {
    const grad = g.createRadialGradient(lobe.x, lobe.y, 0, lobe.x, lobe.y, lobe.r);
    grad.addColorStop(0, "rgba(255,255,255,0.55)");
    grad.addColorStop(0.6, "rgba(248,249,247,0.25)");
    grad.addColorStop(1, "rgba(255,255,255,0)");
    g.fillStyle = grad;
    g.beginPath(); g.arc(lobe.x, lobe.y, lobe.r, 0, Math.PI * 2); g.fill();
  }
  const tex = new THREE.CanvasTexture(c);
  tex.colorSpace = THREE.SRGBColorSpace;
  return tex;
}

function quatAxisAngle(THREE, ax, ay, az, angle) {
  return new THREE.Quaternion().setFromAxisAngle(new THREE.Vector3(ax, ay, az), angle);
}

ctx.onInit = async function (api) {
  const THREE = api.THREE;
  const scene = api.scene;
  const s = api.state;

  // ── Desert skybox + horizon haze ───────────────────────────────────────
  const skybox = new THREE.CubeTextureLoader().load([
    "/assets/skybox/arid2_rt.jpg", "/assets/skybox/arid2_lf.jpg",
    "/assets/skybox/arid2_up.jpg", "/assets/skybox/arid2_dn.jpg",
    "/assets/skybox/arid2_bk.jpg", "/assets/skybox/arid2_ft.jpg",
  ]);
  skybox.colorSpace = THREE.SRGBColorSpace;
  scene.background = skybox;
  scene.fog = new THREE.FogExp2(0xc9d4c4, 0.0006);

  const haze = new THREE.Mesh(
    new THREE.SphereGeometry(4600, 48, 24),
    new THREE.ShaderMaterial({
      uniforms: { hazeColor: { value: new THREE.Color(0xc9d4c4) } },
      vertexShader: `varying vec3 vDir;
        void main(){ vec4 wp = modelMatrix*vec4(position,1.0);
          vDir = normalize(wp.xyz - cameraPosition);
          gl_Position = projectionMatrix*viewMatrix*wp; }`,
      fragmentShader: `uniform vec3 hazeColor; varying vec3 vDir;
        void main(){ float lower = 1.0 - smoothstep(0.02,0.28,vDir.y);
          float core = 1.0 - smoothstep(0.02,0.16,abs(vDir.y));
          float a = max(0.12*core, 0.8*lower);
          if(a<0.01) discard; gl_FragColor = vec4(hazeColor,a); }`,
      transparent: true, depthWrite: false, side: THREE.BackSide,
    })
  );
  haze.renderOrder = 900; scene.add(haze); s.haze = haze;

  // ── Desert lighting (matched to the quadrotor scene) ───────────────────
  const sun = new THREE.DirectionalLight(0xfff0d0, 1.8);
  sun.position.set(80, 120, 40); scene.add(sun);
  const fill = new THREE.DirectionalLight(0xd4a060, 0.4);
  fill.position.set(-50, 30, -40); scene.add(fill);
  const rim = new THREE.DirectionalLight(0xffeebb, 0.25);
  rim.position.set(0, -10, -60); scene.add(rim);
  scene.add(new THREE.HemisphereLight(0x87ceeb, 0xc2956b, 0.5));

  // ── High clouds ────────────────────────────────────────────────────────
  const cloudTex = createCloudTexture(THREE);
  const clouds = new THREE.Group(); scene.add(clouds);
  Array.from({ length: 40 }).forEach((_, i) => {
    const a = deterministicUnit(i * 11 + 201) * Math.PI * 2;
    const r = 600 + deterministicUnit(i * 11 + 202) * 3200;
    const sp = new THREE.Sprite(new THREE.SpriteMaterial({
      map: cloudTex, transparent: true,
      opacity: 0.2 + deterministicUnit(i * 11 + 203) * 0.25, depthWrite: false, fog: true,
    }));
    sp.position.set(Math.cos(a) * r, 220 + deterministicUnit(i * 11 + 204) * 260, Math.sin(a) * r);
    const sc = 280 + deterministicUnit(i * 11 + 205) * 520;
    sp.scale.set(sc * 1.8, sc, 1);
    clouds.add(sp);
  });

  // ── Desert ground (sand PBR) ───────────────────────────────────────────
  const texLoader = new THREE.TextureLoader();
  // Same per-unit texel density as the quadrotor scene (repeat 100 over a 2000
  // plane = 0.05/unit), same material params, scaled up to a 9000 plane.
  const groundMap = (tex, srgb = false) => {
    if (srgb) tex.colorSpace = THREE.SRGBColorSpace;
    tex.wrapS = tex.wrapT = THREE.RepeatWrapping;
    tex.repeat.set(450, 450);
    tex.minFilter = THREE.LinearMipmapLinearFilter;
    tex.magFilter = THREE.LinearFilter;
    tex.anisotropy = api.renderer?.capabilities?.getMaxAnisotropy?.() ?? 1;
    return tex;
  };
  const sandMat = new THREE.MeshStandardMaterial({
    map: groundMap(texLoader.load("/assets/sand_pbr/GroundSand005_COL_1K.jpg"), true),
    normalMap: groundMap(texLoader.load("/assets/sand_pbr/GroundSand005_NRM_1K.jpg")),
    aoMap: groundMap(texLoader.load("/assets/sand_pbr/GroundSand005_AO_1K.jpg")),
    bumpMap: groundMap(texLoader.load("/assets/sand_pbr/GroundSand005_BUMP_1K.jpg")),
    color: 0xffffff, roughness: 0.82, bumpScale: 0.035, metalness: 0.02,
  });
  const floor = new THREE.Mesh(new THREE.PlaneGeometry(9000, 9000), sandMat);
  floor.geometry.setAttribute("uv2", new THREE.BufferAttribute(floor.geometry.attributes.uv.array, 2));
  floor.rotation.x = -Math.PI / 2; scene.add(floor);
  const grid = new THREE.GridHelper(9000, 450, 0xe2bc72, 0xb78945);
  grid.material.transparent = true; grid.material.opacity = 0.18; grid.position.y = 0.05;
  scene.add(grid);

  // ── Desert rocks + cacti (spread wide so the aircraft flies over terrain) ─
  const rockMat = new THREE.MeshStandardMaterial({ color: 0x8b7355, roughness: 0.85 });
  const rockDark = new THREE.MeshStandardMaterial({ color: 0x6b5740, roughness: 0.9 });
  Array.from({ length: 260 }).forEach((_, i) => {
    const a = deterministicUnit(i * 3 + 1) * Math.PI * 2;
    const r = 12 + deterministicUnit(i * 3 + 2) ** 0.6 * 2600;
    const sz = 0.18 + deterministicUnit(i * 3 + 3) * 1.1;
    const tint = deterministicUnit(i * 3 + 6);
    const rock = new THREE.Mesh(new THREE.DodecahedronGeometry(sz, 1), tint > 0.45 ? rockMat : rockDark);
    rock.position.set(Math.cos(a) * r, sz * 0.3, Math.sin(a) * r);
    rock.scale.set(1 + tint * 0.7, 0.4 + tint * 0.35, 0.8 + tint * 0.6);
    rock.rotation.set(0.1, deterministicUnit(i * 3 + 5) * Math.PI, 0.1);
    scene.add(rock);
  });
  const cactusMat = new THREE.MeshStandardMaterial({ color: 0x2f6b35, roughness: 0.82 });
  Array.from({ length: 120 }).forEach((_, i) => {
    const a = deterministicUnit(i * 7 + 101) * Math.PI * 2;
    const r = 18 + deterministicUnit(i * 7 + 102) ** 0.7 * 2800;
    const h = 0.8 + deterministicUnit(i * 7 + 103) * 2.4;
    const cactus = new THREE.Group();
    cactus.position.set(Math.cos(a) * r, 0, Math.sin(a) * r);
    const trunk = new THREE.Mesh(new THREE.CylinderGeometry(0.11, 0.14, h, 8), cactusMat);
    trunk.position.y = h / 2; cactus.add(trunk);
    if (deterministicUnit(i * 7 + 105) > 0.4) {
      const side = deterministicUnit(i * 7 + 106) > 0.5 ? 1 : -1;
      const arm = new THREE.Mesh(new THREE.CylinderGeometry(0.06, 0.08, h * 0.4, 7), cactusMat);
      arm.rotation.z = side * Math.PI / 2; arm.position.set(side * h * 0.18, h * 0.5, 0);
      cactus.add(arm);
    }
    scene.add(cactus);
  });

  // ── Airplane GLB ───────────────────────────────────────────────────────
  const aircraft = new THREE.Group();
  aircraft.name = "aircraft";
  aircraft.matrixAutoUpdate = false;
  scene.add(aircraft);
  s.aircraft = aircraft;

  const loader = new api.GLTFLoader();
  const gltf = await loader.loadAsync("/assets/models/airplane.glb");
  const model = gltf.scene;

  // Scale so 1 GLB unit = 0.16 m (the scale the gear contact offsets in the
  // model assume). Do NOT recenter: keep the GLB's own origin (~CG) at the
  // group origin so the wheels sit at the physics contact height and touch the
  // ground when the model reports it has landed.
  const MODEL_SCALE = 0.16;
  model.scale.setScalar(MODEL_SCALE);
  model.traverse((o) => { if (o.isMesh) { o.castShadow = true; o.receiveShadow = true; } });
  aircraft.add(model);

  // The GLB's *Pivot nodes are empty hinge transforms (correct position +
  // orientation); the surface meshes are separate siblings. Re-parent each mesh
  // under its pivot (preserving world transform), so rotating the pivot deflects
  // the surface about its hinge. Each pivot is pre-rotated ~-90 deg about Y, so
  // in the pivot's LOCAL frame the spanwise hinge is Z (elevator/ailerons), the
  // vertical hinge is Y (rudder), and the nose/spin axis is X (prop).
  const byName = {};
  model.traverse((o) => { byName[o.name] = o; });
  const rig = [
    { pivot: "ElevatorPivot", mesh: "Elevator", ax: [0, 0, 1], sign: 1 },
    { pivot: "LeftAileronPivot", mesh: "LeftAileron", ax: [0, 0, 1], sign: 1 },
    { pivot: "RightAileronPivot", mesh: "RightAileron", ax: [0, 0, 1], sign: -1 },
    { pivot: "RudderPivot", mesh: "Rudder", ax: [0, 1, 0], sign: 1 },
    { pivot: "PropPivot", mesh: "Prop", ax: [1, 0, 0], sign: 1 },
  ];
  s.surfaces = {};
  for (const r of rig) {
    const pv = byName[r.pivot], ms = byName[r.mesh];
    if (!pv || !ms) { console.warn("fixedwing: missing", r.pivot, r.mesh); continue; }
    pv.attach(ms);   // re-parent mesh under pivot, preserving world transform
    s.surfaces[r.pivot] = { node: pv, base: pv.quaternion.clone(), ax: r.ax, sign: r.sign };
  }
  s.propSpin = 0;

  // ── Ground shadow + flight trail ───────────────────────────────────────
  s.shadow = new THREE.Mesh(
    new THREE.CircleGeometry(0.7, 24),
    new THREE.MeshBasicMaterial({ color: 0x2a1d0a, transparent: true, opacity: 0.32 }),
  );
  s.shadow.rotation.x = -Math.PI / 2; s.shadow.position.y = 0.02; scene.add(s.shadow);

  s.MAX_TRAIL = 1500;
  s.trailPos = new Float32Array(s.MAX_TRAIL * 3);
  const trailGeo = new THREE.BufferGeometry();
  trailGeo.setAttribute("position", new THREE.BufferAttribute(s.trailPos, 3));
  trailGeo.setDrawRange(0, 0);
  s.trail = new THREE.Line(trailGeo, new THREE.LineBasicMaterial({ color: 0xffaa33, transparent: true, opacity: 0.7 }));
  scene.add(s.trail);
  s.trailCount = 0;

  // Camera: chase from behind/above (meter scale — aircraft ~1.5 m span).
  api.cam.target.set(0, 1, 0);
  api.cam.dist = 7;
  api.cam.angle = Math.PI;
  api.cam.elev = 0.22;
};

ctx.onFrame = function (api) {
  const THREE = api.THREE;
  const s = api.state;
  const get = api.get;
  if (!s.aircraft) return;

  const px = get("px") ?? 0, py = get("py") ?? 0, pz = get("pz") ?? 100;
  const q0 = get("q0") ?? 1, q1 = get("q1") ?? 0, q2 = get("q2") ?? 0, q3 = get("q3") ?? 0;

  // Body->world DCM (FLU).
  const R11 = 1 - 2 * (q2 * q2 + q3 * q3), R12 = 2 * (q1 * q2 - q0 * q3), R13 = 2 * (q1 * q3 + q0 * q2);
  const R21 = 2 * (q1 * q2 + q0 * q3), R22 = 1 - 2 * (q1 * q1 + q3 * q3), R23 = 2 * (q2 * q3 - q0 * q1);
  const R31 = 2 * (q1 * q3 - q0 * q2), R32 = 2 * (q2 * q3 + q0 * q1), R33 = 1 - 2 * (q1 * q1 + q2 * q2);

  // World (FLU) -> three via A' = {fwd,left,up}->{left,up,fwd}: pos [py,pz,px].
  const tx = py, ty = pz, tz = px;
  // Orientation M' = A' R A'^-1 (airplane mesh is left wing +X).
  s.aircraft.matrix.set(
    R22, R23, R21, tx,
    R32, R33, R31, ty,
    R12, R13, R11, tz,
    0, 0, 0, 1,
  );
  s.aircraft.matrixWorldNeedsUpdate = true;

  // Control-surface deflections (rad), exaggerated slightly for visibility,
  // applied about each pivot's local hinge axis (base * delta = local frame).
  const VIS = 1.6;
  const ail = numericSignal(get("ail_rad")) * VIS;
  const elev = numericSignal(get("elev_rad")) * VIS;
  const rud = numericSignal(get("rud_rad")) * VIS;
  const thr = numericSignal(get("throttle"));
  const setSurf = (name, ang) => {
    const sf = s.surfaces[name];
    if (!sf) return;
    sf.node.quaternion.copy(sf.base).multiply(quatAxisAngle(THREE, sf.ax[0], sf.ax[1], sf.ax[2], sf.sign * ang));
  };
  setSurf("ElevatorPivot", elev);
  setSurf("LeftAileronPivot", ail);
  setSurf("RightAileronPivot", ail);
  setSurf("RudderPivot", rud);
  s.propSpin += 0.6 + thr * 5.0;
  setSurf("PropPivot", s.propSpin);

  // Ground shadow.
  s.shadow.position.set(tx, 0.02, tz);
  const alt = Math.max(ty, 0.05);
  const sc = Math.max(0.35, 1.0 - alt * 0.02);
  s.shadow.scale.set(sc, sc, 1);
  s.shadow.material.opacity = Math.max(0.04, 0.32 - alt * 0.008);

  // Flight trail.
  const idx = s.trailCount % s.MAX_TRAIL;
  s.trailPos[idx * 3] = tx; s.trailPos[idx * 3 + 1] = ty; s.trailPos[idx * 3 + 2] = tz;
  s.trailCount++;
  s.trail.geometry.attributes.position.needsUpdate = true;
  s.trail.geometry.setDrawRange(0, Math.min(s.trailCount, s.MAX_TRAIL));

  // Chase camera — follow position and heading (yaw only).
  const c = api.cam;
  c.target.set(tx, ty, tz);
  const heading = Math.atan2(R21, R11);   // nose direction in three (col2 = R21,R31,R11)
  const ang = c.angle + heading;
  api.camera.position.set(
    tx + c.dist * Math.sin(ang) * Math.cos(c.elev),
    ty + c.dist * Math.sin(c.elev),
    tz + c.dist * Math.cos(ang) * Math.cos(c.elev),
  );
  api.camera.position.y = Math.max(api.camera.position.y, 0.5);
  api.camera.lookAt(c.target);

  if (s.haze) s.haze.position.copy(api.camera.position);
};
