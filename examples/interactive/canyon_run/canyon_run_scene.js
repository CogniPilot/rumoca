// Canyon Run scene for the rumoca sim viewer: a winding sandstone canyon, the
// Courier craft, a reward-per-episode chart and sampled flight paths.
//
// Scene script API:
//   ctx.onInit(api)  — called once after Three.js is ready (may be async)
//   ctx.onFrame(api) — called each animation frame
//   api: { THREE, scene, renderer, state, get(name), camera, cam, GLTFLoader }
//
// Conventions: world Z-up, body FLU, quat {w,x,y,z} body->world. World (FLU)
// maps to three as {x,y,z} -> {y,z,x}, as in the fixed-wing scene.
//
// The canyon shape below must match CanyonRun.Course in CanyonRun.mo.

const RIM_HEIGHT = 30;
const COURSE_LENGTH = 600;
const PATH_EVERY = 5;        // keep the path of every 5th episode
const MAX_PATHS = 12;
const MAX_POINTS = 4000;

function centreLine(x) {
  return 10 * Math.sin(0.0349 * x) + 1.5 * Math.sin(0.0898 * x + 1.3);
}

function halfWidth(x) {
  return 16 + 3 * Math.sin(0.0524 * x + 0.7);
}

function numericSignal(value, fallback = 0) {
  const n = Number(value);
  return Number.isFinite(n) ? n : fallback;
}

function deterministicUnit(seed) {
  const x = Math.sin(seed * 12.9898) * 43758.5453;
  return x - Math.floor(x);
}

// Sandstone colour by height: layered bands with a little noise.
function rockColour(THREE, z, seed) {
  const band = 0.5 + 0.5 * Math.sin(z * 0.9 + 0.6 * Math.sin(z * 0.23));
  const c = new THREE.Color().setHSL(0.06 + 0.02 * band, 0.45 + 0.1 * band, 0.36 + 0.14 * band);
  c.offsetHSL(0, 0, (deterministicUnit(seed) - 0.5) * 0.05);
  return c;
}

// One canyon wall as a strip from the floor to the rim, then out across the
// plateau. side = +1 for the left wall (larger world y), -1 for the right.
function buildWall(THREE, side) {
  const xs = [];
  for (let x = -40; x <= COURSE_LENGTH + 80; x += 2) xs.push(x);
  const heights = [0, 4, 8, 12, 16, 20, 24, RIM_HEIGHT];
  const rows = heights.length + 1;
  const pos = [], col = [], idx = [];
  xs.forEach((x, i) => {
    const y = centreLine(x) + side * halfWidth(x);
    heights.forEach((z, k) => {
      const jag = k === 0 || k === heights.length - 1 ? 0 : 0.6 * (deterministicUnit(i * 13 + k) - 0.5);
      pos.push(y + side * jag, z, x);
      const c = rockColour(THREE, z, i * 7 + k);
      col.push(c.r, c.g, c.b);
    });
    pos.push(y + side * 80, RIM_HEIGHT, x);
    const top = rockColour(THREE, RIM_HEIGHT + 3, i);
    col.push(top.r, top.g, top.b);
  });
  for (let i = 0; i + 1 < xs.length; i++) {
    for (let k = 0; k + 1 < rows; k++) {
      const a = i * rows + k, b = (i + 1) * rows + k;
      if (side > 0) idx.push(a, a + 1, b, b, a + 1, b + 1);
      else idx.push(a, b, a + 1, b, b + 1, a + 1);
    }
  }
  const geo = new THREE.BufferGeometry();
  geo.setAttribute("position", new THREE.Float32BufferAttribute(pos, 3));
  geo.setAttribute("color", new THREE.Float32BufferAttribute(col, 3));
  geo.setIndex(idx);
  geo.computeVertexNormals();
  return new THREE.Mesh(geo, new THREE.MeshStandardMaterial({
    vertexColors: true, roughness: 0.92, metalness: 0.0, side: THREE.DoubleSide, flatShading: true,
  }));
}

// The Courier: a capsule hull, a swept wedge wing, a fin and one thruster.
// Built with local axes forward = +z, left = +x, up = +y.
function buildCourier(THREE) {
  const craft = new THREE.Group();
  const hullMat = new THREE.MeshStandardMaterial({ color: 0xe8edf2, roughness: 0.45, metalness: 0.25 });
  const trimMat = new THREE.MeshStandardMaterial({ color: 0x1f6fb2, roughness: 0.5, metalness: 0.2 });

  const hull = new THREE.Mesh(new THREE.CapsuleGeometry(0.11, 0.7, 6, 14), hullMat);
  hull.rotation.x = Math.PI / 2;
  craft.add(hull);

  const wing = new THREE.Shape();
  wing.moveTo(0, 0.18); wing.lineTo(0.75, -0.12); wing.lineTo(0.75, -0.24);
  wing.lineTo(-0.75, -0.24); wing.lineTo(-0.75, -0.12); wing.closePath();
  const wingGeo = new THREE.ExtrudeGeometry(wing, { depth: 0.025, bevelEnabled: false });
  wingGeo.rotateX(Math.PI / 2);
  const wingMesh = new THREE.Mesh(wingGeo, trimMat);
  wingMesh.position.set(0, 0.0, 0.05);
  craft.add(wingMesh);

  const fin = new THREE.Shape();
  fin.moveTo(0, 0); fin.lineTo(0.22, 0); fin.lineTo(0.05, 0.22); fin.closePath();
  const finGeo = new THREE.ExtrudeGeometry(fin, { depth: 0.02, bevelEnabled: false });
  finGeo.rotateY(-Math.PI / 2);
  const finMesh = new THREE.Mesh(finGeo, trimMat);
  finMesh.position.set(0.01, 0.06, -0.42);
  craft.add(finMesh);

  const nozzle = new THREE.Mesh(new THREE.CylinderGeometry(0.07, 0.09, 0.16, 16, 1, true),
    new THREE.MeshStandardMaterial({ color: 0x3a3f44, roughness: 0.6, metalness: 0.6, side: THREE.DoubleSide }));
  nozzle.rotation.x = Math.PI / 2;
  nozzle.position.set(0, 0, -0.52);
  craft.add(nozzle);

  const flame = new THREE.Mesh(new THREE.ConeGeometry(0.06, 0.4, 14, 1, true),
    new THREE.MeshBasicMaterial({ color: 0x7fd8ff, transparent: true, opacity: 0.7, depthWrite: false }));
  flame.rotation.x = -Math.PI / 2;
  flame.position.set(0, 0, -0.75);
  craft.add(flame);
  return { craft, flame };
}

function createChart(api) {
  const canvas = document.createElement("canvas");
  canvas.width = 340; canvas.height = 170;
  Object.assign(canvas.style, {
    position: "absolute", left: "12px", bottom: "12px", zIndex: 20,
    background: "rgba(20,16,12,0.62)", borderRadius: "6px", pointerEvents: "none",
  });
  (api.renderer?.domElement?.parentElement ?? document.body).appendChild(canvas);
  return canvas;
}

function drawChart(canvas, returns, info) {
  const g = canvas.getContext("2d");
  const w = canvas.width, h = canvas.height, pad = 30;
  g.clearRect(0, 0, w, h);
  g.fillStyle = "#f3e6cf"; g.font = "12px sans-serif";
  g.fillText(`Episode ${info.episode}   epsilon ${info.epsilon.toFixed(2)}   last ${info.lastDistance.toFixed(0)} m`, 10, 16);
  g.fillText("Return per episode", 10, h - 8);
  if (returns.length < 2) return;
  const lo = Math.min(...returns, 0), hi = Math.max(...returns, 1);
  const X = (i) => pad + (i / (returns.length - 1)) * (w - pad - 10);
  const Y = (v) => h - pad - ((v - lo) / (hi - lo)) * (h - 2 * pad - 6);
  g.strokeStyle = "rgba(243,230,207,0.25)";
  g.beginPath(); g.moveTo(pad, Y(0)); g.lineTo(w - 10, Y(0)); g.stroke();
  g.fillStyle = "rgba(255,190,110,0.6)";
  returns.forEach((v, i) => g.fillRect(X(i) - 1, Y(v) - 1, 2, 2));
  g.strokeStyle = "#7fd8ff"; g.lineWidth = 2; g.beginPath();
  let avg = returns[0];
  returns.forEach((v, i) => {
    avg = i === 0 ? v : 0.9 * avg + 0.1 * v;
    if (i === 0) g.moveTo(X(i), Y(avg)); else g.lineTo(X(i), Y(avg));
  });
  g.stroke();
}

ctx.onInit = async function (api) {
  const THREE = api.THREE;
  const scene = api.scene;
  const s = api.state;

  scene.background = new THREE.Color(0xa9c6dd);
  scene.fog = new THREE.Fog(0xc8b89c, 60, 420);
  const sun = new THREE.DirectionalLight(0xfff0d8, 2.0);
  sun.position.set(60, 140, 30); scene.add(sun);
  scene.add(new THREE.HemisphereLight(0xbfd8ee, 0x8a5a36, 0.65));

  const floor = new THREE.Mesh(new THREE.PlaneGeometry(400, COURSE_LENGTH + 200),
    new THREE.MeshStandardMaterial({ color: 0xb98d5c, roughness: 0.95 }));
  floor.rotation.x = -Math.PI / 2;
  floor.position.set(0, 0, COURSE_LENGTH / 2);
  scene.add(floor);
  scene.add(buildWall(THREE, 1));
  scene.add(buildWall(THREE, -1));

  const finish = new THREE.Mesh(new THREE.PlaneGeometry(40, RIM_HEIGHT),
    new THREE.MeshBasicMaterial({ color: 0x7fd8ff, transparent: true, opacity: 0.18, side: THREE.DoubleSide }));
  finish.position.set(centreLine(COURSE_LENGTH), RIM_HEIGHT / 2, COURSE_LENGTH);
  scene.add(finish);

  const { craft, flame } = buildCourier(THREE);
  craft.matrixAutoUpdate = false;
  scene.add(craft);
  s.craft = craft; s.flame = flame;

  s.pathGroup = new THREE.Group(); scene.add(s.pathGroup);
  s.showPaths = true;
  s.current = [];
  s.returns = [];
  s.lastEpisode = 1;
  window.addEventListener("keydown", (e) => {
    if (e.key === "p" || e.key === "P") {
      s.showPaths = !s.showPaths;
      s.pathGroup.visible = s.showPaths;
    }
  });

  s.chart = createChart(api);
  api.cam.target.set(0, 12, 0);
  api.cam.dist = 6;
  api.cam.angle = Math.PI;
  api.cam.elev = 0.18;
};

function keepPath(THREE, s, points, ret) {
  if (points.length < 2) return;
  const geo = new THREE.BufferGeometry().setFromPoints(points);
  const good = Math.max(0, Math.min(1, ret / (0.1 * COURSE_LENGTH)));
  const line = new THREE.Line(geo, new THREE.LineBasicMaterial({
    color: new THREE.Color().setHSL(0.33 * good, 0.8, 0.55), transparent: true, opacity: 0.75,
  }));
  s.pathGroup.add(line);
  while (s.pathGroup.children.length > MAX_PATHS) {
    const old = s.pathGroup.children[0];
    old.geometry.dispose();
    s.pathGroup.remove(old);
  }
}

ctx.onFrame = function (api) {
  const THREE = api.THREE;
  const s = api.state;
  const get = api.get;
  if (!s.craft) return;

  const px = numericSignal(get("px")), py = numericSignal(get("py")), pz = numericSignal(get("pz"), 12);
  const q0 = numericSignal(get("q0"), 1), q1 = numericSignal(get("q1")), q2 = numericSignal(get("q2")), q3 = numericSignal(get("q3"));
  const episode = numericSignal(get("episode"), 1);

  const R11 = 1 - 2 * (q2 * q2 + q3 * q3), R12 = 2 * (q1 * q2 - q0 * q3), R13 = 2 * (q1 * q3 + q0 * q2);
  const R21 = 2 * (q1 * q2 + q0 * q3), R22 = 1 - 2 * (q1 * q1 + q3 * q3), R23 = 2 * (q2 * q3 - q0 * q1);
  const R31 = 2 * (q1 * q3 - q0 * q2), R32 = 2 * (q2 * q3 + q0 * q1), R33 = 1 - 2 * (q1 * q1 + q2 * q2);
  const tx = py, ty = pz, tz = px;
  s.craft.matrix.set(
    R22, R23, R21, tx,
    R32, R33, R31, ty,
    R12, R13, R11, tz,
    0, 0, 0, 1,
  );
  s.craft.matrixWorldNeedsUpdate = true;

  const speed = numericSignal(get("airspeed"), 15);
  s.flame.scale.set(1, 1, Math.max(0.4, Math.min(2.0, 0.6 + 0.12 * (speed - 12))));

  if (episode !== s.lastEpisode) {
    const ret = numericSignal(get("last_return"));
    s.returns.push(ret);
    if (s.returns.length > 2000) s.returns.shift();
    if (s.lastEpisode % PATH_EVERY === 0) keepPath(THREE, s, s.current, ret);
    s.current = [];
    s.lastEpisode = episode;
  }
  if (s.current.length < MAX_POINTS) s.current.push(new THREE.Vector3(tx, ty, tz));

  drawChart(s.chart, s.returns, {
    episode,
    epsilon: numericSignal(get("epsilon"), 1),
    lastDistance: numericSignal(get("last_distance")),
  });

  const c = api.cam;
  c.target.set(tx, ty, tz);
  const heading = Math.atan2(R21, R11);
  const ang = c.angle + heading;
  api.camera.position.set(
    tx + c.dist * Math.sin(ang) * Math.cos(c.elev),
    ty + c.dist * Math.sin(c.elev),
    tz + c.dist * Math.cos(ang) * Math.cos(c.elev),
  );
  api.camera.position.y = Math.max(api.camera.position.y, 0.5);
  api.camera.lookAt(c.target);
};
