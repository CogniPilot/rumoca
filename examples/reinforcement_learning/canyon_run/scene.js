const ctx = {};
// Canyon Run scene for the rumoca sim viewer: a winding sandstone canyon, the
// rigged airplane model of the fixed-wing example, a reward-per-episode chart and
// sampled flight paths.
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

// What the learner sees and what it has learned. The sizes match the Agent defaults in
// CanyonRun.mo, and every value is a viewer signal in rumoca-scenario.toml (occ_k, wL_i_j).
const GRID_ROWS = 8, GRID_COLS = 8, N_FEATURES = 18;
const NET_LAYERS = [86, 64, 64, 4];
const ACTION_NAMES = ["thrust", "p rate", "q rate", "r rate"];
const NET_REFRESH_MS = 500; // weights change once per learning step; redraw at most twice a second

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

// Small procedural textures, drawn once at start-up: no image files to load.
function canvasTexture(THREE, width, height, draw, repeat = true) {
  const canvas = document.createElement("canvas");
  canvas.width = width; canvas.height = height;
  draw(canvas.getContext("2d"), width, height);
  const tex = new THREE.CanvasTexture(canvas);
  if (repeat) tex.wrapS = tex.wrapT = THREE.RepeatWrapping;
  if ("colorSpace" in tex && THREE.SRGBColorSpace) tex.colorSpace = THREE.SRGBColorSpace;
  tex.anisotropy = 1;
  return tex;
}

// Smooth, tileable value noise: a small random grid with a wrapped border, scaled up with
// image smoothing. Summing a few scales gives mottled rock instead of flat colour.
function addNoiseOctave(g, w, h, cells, alpha, seed) {
  const small = document.createElement("canvas");
  small.width = small.height = cells + 1;
  const sg = small.getContext("2d");
  const img = sg.createImageData(cells + 1, cells + 1);
  for (let y = 0; y <= cells; y++) {
    for (let x = 0; x <= cells; x++) {
      const v = Math.round(255 * deterministicUnit(seed + (x % cells) * 12.9 + (y % cells) * 78.2));
      const o = 4 * (y * (cells + 1) + x);
      img.data[o] = img.data[o + 1] = img.data[o + 2] = v; img.data[o + 3] = 255;
    }
  }
  sg.putImageData(img, 0, 0);
  const cw = w / cells, ch = h / cells;
  g.globalAlpha = alpha; g.imageSmoothingEnabled = true;
  g.drawImage(small, 0, 0, cells + 1, cells + 1, -cw / 2, -ch / 2, w + cw, h + ch);
  g.globalAlpha = 1;
}

// Sandstone: mottled grain at several scales, soft wavy sediment layers of varying
// thickness and a few dark streaks running down the face. Fairly light and neutral, so
// the vertex colours set the hue.
function drawRock(g, w, h) {
  g.fillStyle = "rgb(200,186,170)"; g.fillRect(0, 0, w, h);
  g.globalCompositeOperation = "overlay";
  addNoiseOctave(g, w, h, 4, 0.55, 101);
  addNoiseOctave(g, w, h, 8, 0.5, 202);
  addNoiseOctave(g, w, h, 32, 0.35, 303);
  addNoiseOctave(g, w, h, 128, 0.3, 404);
  g.globalCompositeOperation = "source-over";
  let y = 0, layer = 0;
  while (y < h) {
    const thick = 4 + Math.floor(18 * deterministicUnit(layer * 3.1 + 1));
    const dark = deterministicUnit(layer * 7.7 + 2);
    g.fillStyle = dark > 0.5 ? `rgba(70,40,22,${0.08 + 0.14 * (dark - 0.5)})` : `rgba(255,236,210,${0.06 + 0.12 * dark})`;
    g.beginPath(); g.moveTo(0, y);
    for (let x = 0; x <= w; x += 16) g.lineTo(x, y + 2 * Math.sin(x * 0.0491 * 2 + layer));
    for (let x = w; x >= 0; x -= 16) g.lineTo(x, y + thick + 2 * Math.sin(x * 0.0491 * 2 + layer + 1));
    g.closePath(); g.fill();
    y += thick; layer++;
  }
  for (let i = 0; i < 9; i++) {
    const x = deterministicUnit(i * 4.3 + 11) * w, len = h * (0.2 + 0.5 * deterministicUnit(i * 6.1 + 17));
    const grad = g.createLinearGradient(0, 0, 0, len);
    grad.addColorStop(0, "rgba(60,36,24,0.22)"); grad.addColorStop(1, "rgba(60,36,24,0)");
    g.fillStyle = grad; g.fillRect(x, 0, 3 + 6 * deterministicUnit(i * 8.3), len);
  }
}

function drawSand(g, w, h) {
  g.fillStyle = "rgb(214,180,138)"; g.fillRect(0, 0, w, h);
  for (let i = 0; i < 3200; i++) {
    const a = deterministicUnit(i * 1.91 + 21), b = deterministicUnit(i * 3.17 + 23), c = deterministicUnit(i * 0.57 + 29);
    g.fillStyle = c > 0.55 ? `rgba(255,236,205,${0.25 * c})` : `rgba(90,58,34,${0.3 * c})`;
    g.fillRect(Math.floor(a * w), Math.floor(b * h), 1 + Math.floor(2 * c), 1 + Math.floor(2 * c));
  }
  for (let i = 0; i < 18; i++) {
    const r = 6 + 20 * deterministicUnit(i * 7.3 + 31);
    const grad = g.createRadialGradient(0, 0, 0, 0, 0, r);
    grad.addColorStop(0, "rgba(110,72,44,0.35)"); grad.addColorStop(1, "rgba(110,72,44,0)");
    g.save(); g.translate(deterministicUnit(i * 5.1 + 37) * w, deterministicUnit(i * 4.7 + 41) * h);
    g.scale(1, 0.6); g.fillStyle = grad; g.beginPath(); g.arc(0, 0, r, 0, 2 * Math.PI); g.fill(); g.restore();
  }
}

// Equirectangular sky: deep blue overhead to pale haze at the horizon, with
// banks of soft cumulus a little above it.
function drawSky(g, w, h) {
  const grad = g.createLinearGradient(0, 0, 0, h);
  grad.addColorStop(0, "#24548f"); grad.addColorStop(0.3, "#5a88bb");
  grad.addColorStop(0.47, "#a9c0d6"); grad.addColorStop(0.5, "#c3cfd8"); grad.addColorStop(0.53, "#cdbfa8"); grad.addColorStop(1, "#b89a78");
  g.fillStyle = grad; g.fillRect(0, 0, w, h);
  for (let c = 0; c < 22; c++) {
    const cx = deterministicUnit(c * 9.7 + 3) * w, cy = h * (0.16 + 0.2 * deterministicUnit(c * 3.3 + 7));
    const size = 30 + 60 * deterministicUnit(c * 5.5 + 1);
    for (let p = 0; p < 14; p++) {
      const px = cx + (deterministicUnit(c * 41 + p) - 0.5) * size * 2.6;
      const py = cy + (deterministicUnit(c * 43 + p) - 0.7) * size * 0.5;
      const r = size * (0.35 + 0.45 * deterministicUnit(c * 47 + p));
      for (const dx of [-w, 0, w]) {
        const puff = g.createRadialGradient(px + dx, py, 0, px + dx, py, r);
        const shade = py > cy ? "rgba(176,184,196,0.5)" : "rgba(255,255,255,0.8)";
        puff.addColorStop(0, shade); puff.addColorStop(0.55, "rgba(238,242,246,0.42)");
        puff.addColorStop(1, "rgba(220,228,236,0)");
        g.fillStyle = puff; g.beginPath(); g.arc(px + dx, py, r, 0, 2 * Math.PI); g.fill();
      }
    }
  }
}

function drawGlow(g, w, h) {
  const grad = g.createRadialGradient(w / 2, h / 2, 0, w / 2, h / 2, w / 2);
  grad.addColorStop(0, "rgba(255,255,255,1)"); grad.addColorStop(0.25, "rgba(255,200,120,0.85)");
  grad.addColorStop(0.6, "rgba(255,110,40,0.25)"); grad.addColorStop(1, "rgba(255,80,20,0)");
  g.fillStyle = grad; g.fillRect(0, 0, w, h);
}

function drawSmoke(g, w, h) {
  const grad = g.createRadialGradient(w / 2, h / 2, 0, w / 2, h / 2, w / 2);
  grad.addColorStop(0, "rgba(90,80,72,0.9)"); grad.addColorStop(0.55, "rgba(110,98,88,0.45)");
  grad.addColorStop(1, "rgba(120,108,96,0)");
  g.fillStyle = grad; g.fillRect(0, 0, w, h);
}

// Eroded relief behind the true wall surface, in metres. It is never negative, so the
// visible rock never reaches into the corridor the physics flies in: horizontal ledges
// from the sediment layers, buttresses and gullies along the canyon, and some grain.
function wallRecess(x, z, seed) {
  if (z <= 0) return 0;
  const ledge = 0.6 * ((z * 0.31 + 0.25 * Math.sin(x * 0.05 + seed)) % 1);
  const gully = 1.1 * Math.pow(Math.max(0, Math.sin(x * 0.17 + 2.2 * Math.sin(x * 0.031 + seed))), 2);
  return ledge + gully + 0.45 * deterministicUnit(x * 3.7 + z * 11.3 + seed);
}

// One canyon wall as a strip from the floor to an eroded rim, then out across the
// plateau. side = +1 for the left wall (larger world y), -1 for the right.
function buildWall(THREE, side, rockMap) {
  const xs = [];
  for (let x = -40; x <= COURSE_LENGTH + 80; x += 3) xs.push(x);
  const heights = [];
  for (let z = 0; z <= RIM_HEIGHT; z += 3) heights.push(z);
  const plateau = [4, 22, 90];
  const rows = heights.length + plateau.length;
  const seed = side > 0 ? 1.3 : 4.1;
  const pos = [], col = [], uv = [], idx = [];
  xs.forEach((x, i) => {
    const y = centreLine(x) + side * halfWidth(x);
    heights.forEach((z, k) => {
      const top = k === heights.length - 1;
      const zz = top ? z + 1.4 * deterministicUnit(i * 5.3 + seed) : z;
      pos.push(y + side * wallRecess(x, z, seed), zz, x);
      const c = rockColour(THREE, z, i * 7 + k);
      col.push(c.r, c.g, c.b);
      uv.push(x / 16, zz / 8);
    });
    plateau.forEach((out, k) => {
      const rise = k === 2 ? 2 : 1 + 3 * deterministicUnit(i * 2.9 + k * 17 + seed);
      pos.push(y + side * out, RIM_HEIGHT + rise, x);
      const c = rockColour(THREE, RIM_HEIGHT + 2 + k, i * 3 + k).offsetHSL(0, -0.05, 0.06);
      col.push(c.r, c.g, c.b);
      uv.push(x / 16, (RIM_HEIGHT + out) / 8);
    });
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
  geo.setAttribute("uv", new THREE.Float32BufferAttribute(uv, 2));
  geo.setIndex(idx);
  geo.computeVertexNormals();
  return new THREE.Mesh(geo, new THREE.MeshStandardMaterial({
    vertexColors: true, map: rockMap,
    roughness: 0.95, metalness: 0.0, side: THREE.DoubleSide, flatShading: true,
  }));
}

// Distant flat-topped mesas well outside the corridor, for scale and depth through the haze.
function buildMesas(THREE, rockMap) {
  const group = new THREE.Group();
  const material = new THREE.MeshStandardMaterial({ color: 0xc98a5a, map: rockMap, roughness: 1, flatShading: true });
  for (let i = 0; i < 18; i++) {
    const along = -120 + (COURSE_LENGTH + 360) * deterministicUnit(i * 7.1 + 2);
    const side = i % 2 === 0 ? 1 : -1;
    const out = 110 + 260 * deterministicUnit(i * 3.7 + 5);
    const radius = 18 + 40 * deterministicUnit(i * 5.3 + 7);
    const height = 18 + 45 * deterministicUnit(i * 9.1 + 11);
    const mesa = new THREE.Mesh(new THREE.CylinderGeometry(radius * 0.8, radius, height, 7, 2), material);
    mesa.scale.set(1, 1, 0.6 + 0.8 * deterministicUnit(i * 2.3 + 13));
    mesa.rotation.y = deterministicUnit(i * 4.9 + 17) * Math.PI;
    mesa.position.set(centreLine(along) + side * out, RIM_HEIGHT + height / 2 - 2, along);
    group.add(mesa);
  }
  return group;
}

// Wall and floor impacts: a flash, rock and hull debris, then smoke. Effects run on the
// wall clock, so they look the same at any simulation speed; a fixed pool is reused and
// the oldest effect is recycled, so a burst of impacts in Fast mode can never pile up.
const IMPACT_POOL = 3, IMPACT_DEBRIS = 18, IMPACT_SMOKE = 5, IMPACT_SECONDS = 0.6;
function buildImpactPool(THREE, scene) {
  const glow = canvasTexture(THREE, 64, 64, drawGlow, false);
  const smoke = canvasTexture(THREE, 64, 64, drawSmoke, false);
  const shard = new THREE.TetrahedronGeometry(0.3);
  const pool = [];
  for (let e = 0; e < IMPACT_POOL; e++) {
    const group = new THREE.Group();
    group.visible = false;
    const flash = new THREE.Sprite(new THREE.SpriteMaterial({
      map: glow, color: 0xfff0d0, blending: THREE.AdditiveBlending, transparent: true, depthWrite: false,
    }));
    group.add(flash);
    const debrisMaterial = new THREE.MeshStandardMaterial({ roughness: 0.9, transparent: true, emissive: 0xff6a20 });
    const debris = new THREE.InstancedMesh(shard, debrisMaterial, IMPACT_DEBRIS);
    const colour = new THREE.Color();
    const velocities = [];
    for (let k = 0; k < IMPACT_DEBRIS; k++) {
      colour.set(k % 3 === 0 ? 0xdfe6ec : 0x9a6440);
      debris.setColorAt(k, colour);
      const a = deterministicUnit(e * 97 + k * 3.1) * 2 * Math.PI, up = deterministicUnit(e * 89 + k * 5.7);
      const speed = 3 + 6 * deterministicUnit(e * 83 + k * 7.9);
      velocities.push(new THREE.Vector3(Math.cos(a) * speed, 2 + 6 * up, Math.sin(a) * speed));
    }
    group.add(debris);
    const puffs = [];
    for (let k = 0; k < IMPACT_SMOKE; k++) {
      const puff = new THREE.Sprite(new THREE.SpriteMaterial({ map: smoke, transparent: true, depthWrite: false }));
      group.add(puff);
      puffs.push(puff);
    }
    scene.add(group);
    pool.push({ group, flash, debris, velocities, puffs, startedAt: -Infinity });
  }
  return pool;
}

function startImpact(pool, x, y, z, now) {
  const effect = pool.reduce((a, b) => (b.startedAt < a.startedAt ? b : a));
  effect.group.position.set(x, y, z);
  effect.startedAt = now;
  effect.group.visible = true;
}

function clearImpacts(pool) {
  for (const effect of pool) { effect.group.visible = false; effect.startedAt = -Infinity; }
}

function animateImpacts(THREE, pool, now, matrix) {
  for (const effect of pool) {
    if (!effect.group.visible) continue;
    const age = (now - effect.startedAt) / 1000;
    if (age > IMPACT_SECONDS) { effect.group.visible = false; continue; }
    const f = age / IMPACT_SECONDS;
    effect.flash.scale.setScalar(1 + 9 * Math.min(1, age / 0.12));
    effect.flash.material.opacity = Math.max(0, 1 - age / 0.22);
    effect.debris.material.opacity = 1 - f * f;
    effect.debris.material.emissiveIntensity = Math.max(0, 1.6 * (1 - age / 0.3));
    for (let k = 0; k < IMPACT_DEBRIS; k++) {
      const v = effect.velocities[k];
      matrix.makeRotationY(7 * age + k);
      matrix.setPosition(v.x * age, v.y * age - 4.9 * age * age, v.z * age);
      effect.debris.setMatrixAt(k, matrix);
    }
    effect.debris.instanceMatrix.needsUpdate = true;
    effect.puffs.forEach((puff, k) => {
      const lag = k * 0.04;
      const t = Math.max(0, age - lag);
      puff.position.set(Math.sin(k * 2.4) * 0.8 * t * 3, 0.6 + 2.2 * t, Math.cos(k * 2.4) * 0.8 * t * 3);
      puff.scale.setScalar(2 + 10 * t);
      puff.material.opacity = t > 0 ? 0.65 * (1 - f) : 0;
    });
  }
}

// The fixed-wing example's rigged airplane (nose +Z, up +Y, left wing +X; 1 GLB unit =
// 0.16 m). Each control-surface mesh is re-parented under its hinge pivot, so rotating the
// pivot deflects the surface, exactly as in fixedwing_scene.js.
async function loadAirplane(api, craft) {
  const THREE = api.THREE;
  const gltf = await new api.GLTFLoader().loadAsync("/assets/models/airplane.glb");
  const model = gltf.scene;
  model.scale.setScalar(0.16);
  craft.add(model);
  const byName = {};
  model.traverse((o) => { byName[o.name] = o; });
  const surfaces = {};
  for (const r of [
    { pivot: "ElevatorPivot", mesh: "Elevator", ax: [0, 0, 1], sign: 1 },
    { pivot: "LeftAileronPivot", mesh: "LeftAileron", ax: [0, 0, 1], sign: 1 },
    { pivot: "RightAileronPivot", mesh: "RightAileron", ax: [0, 0, 1], sign: -1 },
    { pivot: "RudderPivot", mesh: "Rudder", ax: [0, 1, 0], sign: 1 },
    { pivot: "PropPivot", mesh: "Prop", ax: [1, 0, 0], sign: 1 },
  ]) {
    const pivot = byName[r.pivot], mesh = byName[r.mesh];
    if (!pivot || !mesh) { console.warn("canyon_run: missing", r.pivot, r.mesh); continue; }
    pivot.attach(mesh);
    surfaces[r.pivot] = { node: pivot, base: pivot.quaternion.clone(), axis: new THREE.Vector3(...r.ax), sign: r.sign };
  }
  return surfaces;
}

function setSurface(THREE, s, name, angle) {
  const surface = s.surfaces[name];
  if (!surface) return;
  s.hinge ??= new THREE.Quaternion();
  surface.node.quaternion.copy(surface.base).multiply(s.hinge.setFromAxisAngle(surface.axis, surface.sign * angle));
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

function createPanel(api, width, height, place) {
  const canvas = document.createElement("canvas");
  canvas.width = width; canvas.height = height;
  Object.assign(canvas.style, {
    position: "absolute", zIndex: 20, background: "rgba(20,16,12,0.62)",
    borderRadius: "6px", pointerEvents: "none", ...place,
  });
  (api.renderer?.domElement?.parentElement ?? document.body).appendChild(canvas);
  return canvas;
}

// Keep the overlays above the viewer's control bar (which can wrap to two rows) and shrink
// them together when the viewer is narrow or short, so every control stays visible.
const OVERLAY_MARGIN = 12, OVERLAY_GAP = 8;
function layoutOverlays(api, s) {
  const host = s.chart.parentElement;
  if (!host) return;
  const controls = (host.closest(".rumoca-interactive-root") ?? host).querySelector(".rumoca-interactive-controls");
  const box = host.getBoundingClientRect();
  const w = box.width, h = box.height;
  const bar = controls && controls.offsetHeight > 0 ? controls.getBoundingClientRect() : null;
  const barTop = bar ? bar.top - box.top : h - OVERLAY_MARGIN;
  const key = `${w}x${h}x${barTop}x${!!api.replayMode}`;
  if (key === s.layoutKey) return;
  s.layoutKey = key;
  const bottom = Math.max(OVERLAY_MARGIN, h - barTop) + OVERLAY_GAP;
  const leftW = Math.max(s.chart.width, s.grid.width), leftH = (api.replayMode ? 0 : s.chart.height + OVERLAY_GAP) + s.grid.height;
  const scale = Math.max(0.4, Math.min(1,
    (w - 3 * OVERLAY_MARGIN) / (leftW + s.net.width),
    (h - bottom - OVERLAY_MARGIN) / Math.max(leftH, s.net.height)));
  for (const c of [s.chart, s.grid, s.net]) {
    c.style.width = `${Math.round(c.width * scale * (c === s.net ? 0.8 : 1))}px`;
    c.style.height = `${Math.round(c.height * scale * (c === s.net ? 0.8 : 1))}px`;
  }
  s.chart.style.bottom = `${bottom}px`;
  s.grid.style.bottom = `${bottom + (api.replayMode ? 0 : Math.round((s.chart.height + OVERLAY_GAP) * scale))}px`;
  s.net.style.bottom = `${bottom}px`;
}

// The 8 x 8 occupancy grid the network is given: row 1 is farthest ahead, column 1 leftmost,
// and each cell is the share of its sample points outside the canyon (1 = wall).
function drawOccupancy(canvas, get) {
  const g = canvas.getContext("2d");
  const w = canvas.width, h = canvas.height, top = 22, pad = 10;
  const cell = Math.min((w - 2 * pad) / GRID_COLS, (h - top - pad) / GRID_ROWS);
  const left = (w - cell * GRID_COLS) / 2;
  g.clearRect(0, 0, w, h);
  g.fillStyle = "#f3e6d0"; g.font = "12px sans-serif";
  g.fillText("What the agent sees (ahead \u2191)", pad, 15);
  for (let i = 0; i < GRID_ROWS; i++) {
    for (let j = 0; j < GRID_COLS; j++) {
      const v = Math.max(0, Math.min(1, numericSignal(get(`occ_${i * GRID_COLS + j + 1}`))));
      g.fillStyle = `rgb(${Math.round(70 + 120 * v)}, ${Math.round(150 - 90 * v)}, ${Math.round(190 - 150 * v)})`;
      g.fillRect(left + j * cell + 1, top + i * cell + 1, cell - 2, cell - 2);
    }
  }
  g.fillStyle = "#ffd27a";
  g.beginPath();
  const cx = left + cell * GRID_COLS / 2, cy = top + cell * GRID_ROWS + 1;
  g.moveTo(cx, cy - 7); g.lineTo(cx - 5, cy + 2); g.lineTo(cx + 5, cy + 2); g.fill();
}

function weightColour(weight, scale) {
  const a = 0.06 + 0.8 * Math.min(1, Math.abs(weight) / scale);
  return weight >= 0 ? `rgba(255,150,60,${a.toFixed(3)})` : `rgba(90,170,255,${a.toFixed(3)})`;
}

// The Q-network as a layer diagram: every line is one weight, orange positive and blue negative,
// brighter for larger magnitude (scaled per layer). Inputs show the 15 flight features, then the
// 64 grid cells shaded by occupancy; the chosen action is ringed.
function drawNetwork(canvas, get, action) {
  const g = canvas.getContext("2d");
  const w = canvas.width, h = canvas.height, top = 24, bottom = 10, left = 18, right = 88;
  g.clearRect(0, 0, w, h);
  g.fillStyle = "#f3e6d0"; g.font = "12px sans-serif";
  g.fillText("PPO actor 86 → 64 → 64 → 4 (orange +, blue −)", 10, 16);
  const xs = NET_LAYERS.map((_, l) => left + (w - left - right) * l / (NET_LAYERS.length - 1));
  const ys = NET_LAYERS.map((n) => Array.from({ length: n }, (_, k) => top + (h - top - bottom) * (n === 1 ? 0.5 : k / (n - 1))));
  for (let l = 0; l < NET_LAYERS.length - 1; l++) {
    const rows = NET_LAYERS[l + 1], cols = NET_LAYERS[l];
    const weights = new Float64Array(rows * cols);
    let scale = 1e-9;
    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        const v = numericSignal(get(`w${l + 1}_${i + 1}_${j + 1}`));
        weights[i * cols + j] = v;
        scale = Math.max(scale, Math.abs(v));
      }
    }
    g.lineWidth = l === 0 ? 0.5 : 1;
    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        g.strokeStyle = weightColour(weights[i * cols + j], scale);
        g.beginPath(); g.moveTo(xs[l], ys[l][j]); g.lineTo(xs[l + 1], ys[l + 1][i]); g.stroke();
      }
    }
  }
  for (let l = 0; l < NET_LAYERS.length; l++) {
    for (let k = 0; k < NET_LAYERS[l]; k++) {
      if (l === 0 && k >= N_FEATURES && k < N_FEATURES + 64) {
        const v = Math.max(0, Math.min(1, numericSignal(get(`occ_${k - N_FEATURES + 1}`))));
        g.fillStyle = `rgb(${Math.round(70 + 120 * v)}, ${Math.round(150 - 90 * v)}, ${Math.round(190 - 150 * v)})`;
      } else {
        g.fillStyle = l === 0 ? "#b9ad9b" : "#f3e6d0";
      }
      g.beginPath(); g.arc(xs[l], ys[l][k], l === 0 ? 1.6 : 2.6, 0, 2 * Math.PI); g.fill();
    }
  }
  const out = NET_LAYERS.length - 1;
  g.font = "11px sans-serif";
  for (let k = 0; k < NET_LAYERS[out]; k++) {
    const chosen = false;
    g.fillStyle = chosen ? "#ffd27a" : "#cbbfae";
    g.fillText(`${ACTION_NAMES[k]} ${numericSignal(get(`control_${k}`)).toFixed(2)}`, xs[out] + 10, ys[out][k] + 4);
    if (chosen) {
      g.strokeStyle = "#ffd27a"; g.lineWidth = 2;
      g.beginPath(); g.arc(xs[out], ys[out][k], 6, 0, 2 * Math.PI); g.stroke();
    }
  }
}

function drawChart(canvas, returns, info) {
  const g = canvas.getContext("2d");
  const w = canvas.width, h = canvas.height, pad = 30;
  g.clearRect(0, 0, w, h);
  g.fillStyle = "#f3e6cf"; g.font = "12px sans-serif";
  g.fillText(`Episode ${info.episode}   PPO   last ${info.lastDistance.toFixed(0)} m`, 10, 16);
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

  // Blue-grey distance haze, a low warm sun that lights one wall and leaves the other in
  // cool shade, and a sky dome that follows the camera.
  scene.background = new THREE.Color(0xb9c8d6);
  scene.fog = new THREE.Fog(0xb7c5d2, 80, 620);
  const sun = new THREE.DirectionalLight(0xffdcae, 3.1);
  sun.position.set(-110, 80, 70); scene.add(sun);
  scene.add(new THREE.HemisphereLight(0xa9c3e0, 0x8a5a36, 0.95));
  s.sky = new THREE.Mesh(new THREE.SphereGeometry(1400, 32, 16), new THREE.MeshBasicMaterial({
    map: canvasTexture(THREE, 1024, 512, drawSky, false), side: THREE.BackSide, fog: false, depthWrite: false,
  }));
  s.sky.renderOrder = -1;
  scene.add(s.sky);
  if (api.camera.far < 3000) { api.camera.far = 3000; api.camera.updateProjectionMatrix(); }

  const rockMap = canvasTexture(THREE, 256, 256, drawRock);
  const sandMap = canvasTexture(THREE, 256, 256, drawSand);
  sandMap.repeat.set(40, 80);
  const floor = new THREE.Mesh(new THREE.PlaneGeometry(400, COURSE_LENGTH + 200),
    new THREE.MeshStandardMaterial({ color: 0xc8a070, map: sandMap, roughness: 1 }));
  floor.rotation.x = -Math.PI / 2;
  floor.position.set(0, 0, COURSE_LENGTH / 2);
  scene.add(floor);
  scene.add(buildWall(THREE, 1, rockMap));
  scene.add(buildWall(THREE, -1, rockMap));
  scene.add(buildMesas(THREE, rockMap));
  s.impacts = buildImpactPool(THREE, scene);
  s.impactMatrix = new THREE.Matrix4();
  s.lastImpacts = 0;
  s.holdUntil = -Infinity;
  s.lastHoldAt = -Infinity;

  const finish = new THREE.Mesh(new THREE.PlaneGeometry(40, RIM_HEIGHT),
    new THREE.MeshBasicMaterial({ color: 0x7fd8ff, transparent: true, opacity: 0.18, side: THREE.DoubleSide }));
  finish.position.set(centreLine(COURSE_LENGTH), RIM_HEIGHT / 2, COURSE_LENGTH);
  scene.add(finish);

  const craft = new THREE.Group();
  craft.matrixAutoUpdate = false;
  scene.add(craft);
  s.surfaces = await loadAirplane(api, craft);
  s.propSpin = 0;
  s.craft = craft;

  s.pathGroup = new THREE.Group(); scene.add(s.pathGroup);
  s.showPaths = true;
  s.current = [];
  s.returns = [];
  s.lastEpisode = 1;
  s.showNet = true;
  window.addEventListener("keydown", (e) => {
    if (e.key === "p" || e.key === "P") {
      s.showPaths = !s.showPaths;
      s.pathGroup.visible = s.showPaths;
    }
    if (e.key === "n" || e.key === "N") {
      s.showNet = !s.showNet;
      s.grid.style.display = "";
      s.net.style.display = s.showNet ? "" : "none";
    }
  });

  s.chart = createChart(api);
  s.grid = createPanel(api, 216, 200, { left: "12px", bottom: "194px" });
  s.net = createPanel(api, 420, 300, { right: "12px", bottom: "12px" });
  s.chart.dataset.panel = 'episode-returns';
  s.grid.dataset.panel = 'occupancy';
  s.net.dataset.panel = 'actor-network';
  s.netDrawnAt = -Infinity;
  s.layoutCheckedAt = -Infinity;
  layoutOverlays(api, s);
  api.cam.target.set(0, 12, 0);
  api.cam.dist = 3.6;
  api.cam.angle = Math.PI;
  api.cam.elev = 0.14;
};

// Keep a chase camera that is below the rim inside the flyable corridor, so it never
// renders from inside the rock. Three.js x is world y (across), z is world x (along).
function keepCameraInCanyon(camera) {
  const p = camera.position;
  p.y = Math.max(p.y, 0.5);
  if (p.y >= RIM_HEIGHT) return;
  const mid = centreLine(p.z), half = halfWidth(p.z) - 0.8;
  p.x = Math.max(mid - half, Math.min(mid + half, p.x));
}

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
  s.chart.style.display = api.replayMode ? 'none' : '';

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

  // What the fly-by-wire actually commands, exaggerated a little to be visible.
  const VIS = 1.6;
  setSurface(THREE, s, "ElevatorPivot", numericSignal(get("elev_rad")) * VIS);
  setSurface(THREE, s, "LeftAileronPivot", numericSignal(get("ail_rad")) * VIS);
  setSurface(THREE, s, "RightAileronPivot", numericSignal(get("ail_rad")) * VIS);
  setSurface(THREE, s, "RudderPivot", numericSignal(get("rud_rad")) * VIS);
  s.propSpin += 0.6 + 5 * numericSignal(get("throttle"));
  setSurface(THREE, s, "PropPivot", s.propSpin);

  if (episode !== s.lastEpisode) {
    const ret = numericSignal(get("last_return"));
    s.returns.push(ret);
    if (s.returns.length > 2000) s.returns.shift();
    if (s.lastEpisode % PATH_EVERY === 0) keepPath(THREE, s, s.current, ret);
    s.current = [];
    s.lastEpisode = episode;
  }
  if (s.current.length < MAX_POINTS) s.current.push(new THREE.Vector3(tx, ty, tz));

  const layoutNow = performance.now();
  if (layoutNow - s.layoutCheckedAt >= 250) {
    layoutOverlays(api, s);
    s.layoutCheckedAt = layoutNow;
  }
  if (api.episodeReturns) s.returns = api.episodeReturns.slice(-2000);
  drawChart(s.chart, s.returns, {
    episode,
    epsilon: numericSignal(get("epsilon"), 1),
    lastDistance: numericSignal(get("last_distance")),
  });
  drawOccupancy(s.grid, get);
  if (s.showNet) {
    const now = performance.now();
    if (now - s.netDrawnAt >= NET_REFRESH_MS) {
      drawNetwork(s.net, get, Math.round(numericSignal(get("action"), 1)));
      s.netDrawnAt = now;
    }
  }

  // A new impact since the last frame (several between frames show as one, at the latest);
  // a smaller count means the session was restarted, so stale effects are cleared.
  const frameNow = performance.now();
  const impacts = numericSignal(get("impacts"), NaN);
  if (Number.isFinite(impacts)) {
    if (impacts < s.lastImpacts) {
      clearImpacts(s.impacts);
      s.holdUntil = -Infinity;
    } else if (impacts > s.lastImpacts) {
      const ix = numericSignal(get("impact_y")), iy = numericSignal(get("impact_z")), iz = numericSignal(get("impact_x"));
      startImpact(s.impacts, ix, iy, iz, frameNow);
      // Hold the camera on the impact briefly while training carries on, at most every
      // two seconds so Fast mode keeps following the aircraft.
      if (frameNow - s.lastHoldAt > 2000) {
        s.holdUntil = frameNow + 450;
        s.lastHoldAt = frameNow;
        // Frame it from part-way back towards the middle of the canyon, a little behind and above.
        const back = Math.max(0, iz - 6), mid = centreLine(back);
        s.holdFrom = new THREE.Vector3(mid + 0.35 * (ix - mid), iy + 1.5, back);
        keepCameraInCanyon({ position: s.holdFrom });
        s.holdAt = new THREE.Vector3(ix, iy, iz);
      }
    }
    s.lastImpacts = impacts;
  }
  animateImpacts(THREE, s.impacts, frameNow, s.impactMatrix);

  const c = api.cam;
  if (frameNow < s.holdUntil) {
    api.camera.position.copy(s.holdFrom);
    api.camera.lookAt(s.holdAt);
  } else {
    // Close behind the craft, looking a little ahead of it so the route stays in view.
    c.target.set(tx, ty, tz);
    const heading = Math.atan2(R21, R11);
    const ang = c.angle + heading;
    api.camera.position.set(
      tx + c.dist * Math.sin(ang) * Math.cos(c.elev),
      ty + c.dist * Math.sin(c.elev),
      tz + c.dist * Math.cos(ang) * Math.cos(c.elev),
    );
    keepCameraInCanyon(api.camera);
    const ahead = 2.5;
    api.camera.lookAt(tx - ahead * Math.sin(ang), ty + 0.35, tz - ahead * Math.cos(ang));
  }
  s.sky.position.copy(api.camera.position);
};

export { ctx };
