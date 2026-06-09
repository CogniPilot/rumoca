// 3D replay for the PerchReplay sim — a primitive fixed-wing aircraft flying the
// MPC-optimal perching arc. Built from Three.js primitives (no GLB), so it runs
// directly in the embedded results panel just like the ball example.
//
// The Perch plant is a 2-D longitudinal model in the world x-z plane (z up):
//   plant.x      world x position  [m]  (forward)
//   plant.z      world z position  [m]  (up)
//   plant.theta  pitch angle       [rad] (0 = nose level, + = nose up)
//   plant.delta_e elevator         [rad] (+ = nose up)
// Mapping to Three.js (y up): world x -> three x, world z -> three y, lateral
// (wingspan) -> three z. Pitch is a rotation about the lateral axis (three z):
// +theta turns the nose (+x) toward up (+y).

function num(value, fallback) {
  const n = Number(value);
  return Number.isFinite(n) ? n : (fallback ?? 0);
}

ctx.onInit = (api) => {
  if (typeof api.enableDefaultViewerRuntime === "function") {
    api.enableDefaultViewerRuntime({ selectedObjectName: "plane", followSelected: true });
  }
  const { THREE, state } = api;
  if (!THREE || !state || !state.scene) return;
  const scene = state.scene;

  // ── Sky + lighting ──────────────────────────────────────────────────────
  scene.background = new THREE.Color(0x0e1726);
  scene.fog = new THREE.FogExp2(0x0e1726, 0.006);

  const key = new THREE.DirectionalLight(0xffffff, 1.4);
  key.position.set(20, 40, 25);
  scene.add(key);
  const fill = new THREE.DirectionalLight(0x88aaff, 0.4);
  fill.position.set(-30, 10, -20);
  scene.add(fill);
  scene.add(new THREE.HemisphereLight(0x9fc6ff, 0x202830, 0.7));

  // ── Ground at the perch / landing elevation (the arc descends ~18 m) ──────
  const GROUND_Y = -18;
  const floor = new THREE.Mesh(
    new THREE.PlaneGeometry(200, 200),
    new THREE.MeshStandardMaterial({ color: 0x1b2a1f, roughness: 0.95 })
  );
  floor.rotation.x = -Math.PI / 2;
  floor.position.set(40, GROUND_Y, 0);
  floor.name = "floor";
  scene.add(floor);
  const grid = new THREE.GridHelper(200, 80, 0x2f6f4f, 0x24402f);
  grid.position.set(40, GROUND_Y + 0.02, 0);
  scene.add(grid);

  // ── Perch target: a little post + crossbar at the maneuver's end point ────
  const perch = new THREE.Group();
  perch.position.set(52.23, GROUND_Y, 0);
  const postMat = new THREE.MeshStandardMaterial({ color: 0x8a6b43, roughness: 0.8 });
  const post = new THREE.Mesh(new THREE.CylinderGeometry(0.12, 0.16, 18, 10), postMat);
  post.position.y = 9;
  perch.add(post);
  const bar = new THREE.Mesh(new THREE.CylinderGeometry(0.08, 0.08, 2.4, 8), postMat);
  bar.rotation.x = Math.PI / 2;
  bar.position.y = 18;
  perch.add(bar);
  scene.add(perch);

  // ── Primitive aircraft (nose +x, wingspan along three z) ─────────────────
  const plane = new THREE.Group();
  plane.name = "plane";
  scene.add(plane);

  const bodyMat = new THREE.MeshStandardMaterial({ color: 0x3cb4ff, roughness: 0.5, metalness: 0.1 });
  const wingMat = new THREE.MeshStandardMaterial({ color: 0xe8eef5, roughness: 0.6 });
  const tailMat = new THREE.MeshStandardMaterial({ color: 0xff8a3c, roughness: 0.6 });

  // Fuselage along +x.
  const fuselage = new THREE.Mesh(new THREE.BoxGeometry(2.4, 0.34, 0.34), bodyMat);
  plane.add(fuselage);
  // Nose cone.
  const nose = new THREE.Mesh(new THREE.ConeGeometry(0.2, 0.6, 16), bodyMat);
  nose.rotation.z = -Math.PI / 2; // tip toward +x
  nose.position.set(1.4, 0, 0);
  plane.add(nose);

  // Main wing: chord along x, span along z.
  const wing = new THREE.Mesh(new THREE.BoxGeometry(0.62, 0.06, 3.2), wingMat);
  wing.position.set(0.25, 0.08, 0);
  plane.add(wing);

  // Horizontal stabilizer near the tail.
  const hstab = new THREE.Mesh(new THREE.BoxGeometry(0.4, 0.05, 1.3), tailMat);
  hstab.position.set(-1.0, 0.05, 0);
  plane.add(hstab);

  // Elevator — hinged at the stabilizer trailing edge, deflects with delta_e.
  const elevatorPivot = new THREE.Group();
  elevatorPivot.position.set(-1.18, 0.05, 0);
  const elevator = new THREE.Mesh(new THREE.BoxGeometry(0.22, 0.04, 1.3), tailMat);
  elevator.position.set(-0.11, 0, 0); // extend aft of the hinge
  elevatorPivot.add(elevator);
  plane.add(elevatorPivot);
  state.elevatorPivot = elevatorPivot;

  // Vertical fin.
  const fin = new THREE.Mesh(new THREE.BoxGeometry(0.5, 0.7, 0.05), tailMat);
  fin.position.set(-1.05, 0.4, 0);
  plane.add(fin);

  // Spinning prop disc at the nose.
  const prop = new THREE.Mesh(
    new THREE.BoxGeometry(0.04, 1.0, 0.12),
    new THREE.MeshStandardMaterial({ color: 0x222831, roughness: 0.4 })
  );
  prop.position.set(1.72, 0, 0);
  plane.add(prop);
  state.prop = prop;
  state.propSpin = 0;

  state.plane = plane;

  // ── Full flight path (faint) + a brighter "flown-so-far" trail ───────────
  const n = (api.times && api.times.length) ? api.times.length : 0;
  const pathPts = [];
  for (let i = 0; i < n; i++) {
    pathPts.push(new THREE.Vector3(
      num(api.getValue("plant.x", i)),
      num(api.getValue("plant.z", i)),
      0
    ));
  }
  if (pathPts.length > 1) {
    const fullGeo = new THREE.BufferGeometry().setFromPoints(pathPts);
    const fullPath = new THREE.Line(
      fullGeo,
      new THREE.LineBasicMaterial({ color: 0x3a5a7a, transparent: true, opacity: 0.5 })
    );
    scene.add(fullPath);
  }
  state.pathPts = pathPts;
  state.trailPos = new Float32Array(Math.max(2, pathPts.length) * 3);
  const trailGeo = new THREE.BufferGeometry();
  trailGeo.setAttribute("position", new THREE.BufferAttribute(state.trailPos, 3));
  trailGeo.setDrawRange(0, 0);
  state.trail = new THREE.Line(
    trailGeo,
    new THREE.LineBasicMaterial({ color: 0xffaa33, transparent: true, opacity: 0.9 })
  );
  scene.add(state.trail);
};

ctx.onFrame = (api) => {
  const { state } = api;
  const plane = state ? state.plane : null;
  if (!plane) return;
  const i = api.sampleIndex;

  const x = num(api.getValue("plant.x", i));
  const z = num(api.getValue("plant.z", i));
  const theta = num(api.getValue("plant.theta", i));
  const dele = num(api.getValue("plant.delta_e", i));
  const thrust = num(api.getValue("plant.thrust", i));

  // World (x, z up) -> three (x, y); pitch about the lateral axis (three z).
  plane.position.set(x, z, 0);
  plane.rotation.set(0, 0, theta);

  // Elevator deflection (exaggerated for visibility); + delta_e = nose-up =
  // trailing edge up = pivot rotates +z in the plane's local frame.
  if (state.elevatorPivot) state.elevatorPivot.rotation.z = dele * 1.5;

  // Prop spin scales with thrust.
  if (state.prop) {
    state.propSpin += 0.3 + thrust * 0.05;
    state.prop.rotation.x = state.propSpin;
  }

  // Grow the bright trail up to the current sample.
  if (state.trail && state.pathPts) {
    const count = Math.min(i + 1, state.pathPts.length);
    for (let k = 0; k < count; k++) {
      const p = state.pathPts[k];
      state.trailPos[k * 3] = p.x;
      state.trailPos[k * 3 + 1] = p.y;
      state.trailPos[k * 3 + 2] = p.z;
    }
    state.trail.geometry.attributes.position.needsUpdate = true;
    state.trail.geometry.setDrawRange(0, count);
  }
};
