// Rover scene for rumoca sim run viewer.
//
// Expects the following state keys (from rover.toml `[signals.viewer]`):
//   x, y, theta         — planar pose, radians
//   wheel_left, wheel_right — wheel angular velocities [rad/s]

ctx.onInit = function(api) {
  const THREE = api.THREE;
  const scene = api.scene;
  const s = api.state;

  // ── Lights ────────────────────────────────────────────────────────────
  scene.add(new THREE.HemisphereLight(0xbfd8ff, 0x404060, 0.9));
  const sun = new THREE.DirectionalLight(0xffffff, 1.2);
  sun.position.set(6, 10, 4);
  scene.add(sun);

  // ── Ground ────────────────────────────────────────────────────────────
  const ground = new THREE.Mesh(
    new THREE.PlaneGeometry(50, 50),
    new THREE.MeshStandardMaterial({ color: 0x6a8040, roughness: 0.95 })
  );
  ground.rotation.x = -Math.PI / 2;
  scene.add(ground);
  const grid = new THREE.GridHelper(50, 50, 0x3a5020, 0x3a5020);
  grid.material.transparent = true;
  grid.material.opacity = 0.35;
  scene.add(grid);

  // ── Rover group (root transform) ─────────────────────────────────────
  s.rover = new THREE.Group();
  scene.add(s.rover);

  // Body — a box, slightly raised so wheels stick below.
  const body = new THREE.Mesh(
    new THREE.BoxGeometry(0.6, 0.2, 0.4),
    new THREE.MeshStandardMaterial({ color: 0xaa2222, roughness: 0.5 })
  );
  body.position.y = 0.15;
  s.rover.add(body);

  // Heading arrow (so you can see which way is "forward").
  const arrow = new THREE.Mesh(
    new THREE.ConeGeometry(0.08, 0.2, 8),
    new THREE.MeshStandardMaterial({ color: 0xffcc33 })
  );
  arrow.rotation.z = -Math.PI / 2; // point along +X
  arrow.position.set(0.35, 0.25, 0);
  s.rover.add(arrow);

  // Four wheels. Positions: front/rear on X, left/right on Z.
  const wheelGeo = new THREE.CylinderGeometry(0.1, 0.1, 0.05, 16);
  wheelGeo.rotateX(Math.PI / 2); // align axis along Z
  const wheelMat = new THREE.MeshStandardMaterial({ color: 0x222222, roughness: 0.6 });
  s.wheels = { left: [], right: [] };
  const wheelSpots = [
    { x:  0.22, z:  0.22, side: "left"  },
    { x:  0.22, z: -0.22, side: "right" },
    { x: -0.22, z:  0.22, side: "left"  },
    { x: -0.22, z: -0.22, side: "right" },
  ];
  for (const spot of wheelSpots) {
    const w = new THREE.Mesh(wheelGeo, wheelMat);
    w.position.set(spot.x, 0.1, spot.z);
    s.rover.add(w);
    s.wheels[spot.side].push(w);
  }

  // Track cumulative wheel rotation for animation.
  s.leftAngle = 0;
  s.rightAngle = 0;
  s.lastT = 0;

  // Camera: chase view — follow the rover from behind and above.
  api.cam.dist = 4;
  api.cam.elev = 0.9;
  api.cam.angle = 0;
};

ctx.onFrame = function(api) {
  const s = api.state;
  if (!s.rover) return;

  const x = api.get("x") || 0;
  const y = api.get("y") || 0;
  const theta = api.get("theta") || 0;
  const wL = api.get("wheel_left") || 0;
  const wR = api.get("wheel_right") || 0;
  const t = api.get("t") || 0;

  // World (x, y) → Three.js (x, -z); theta around Y is -theta (right-handed).
  s.rover.position.set(x, 0, -y);
  s.rover.rotation.y = -theta;

  // Animate wheels from angular velocity.
  const dt = Math.max(0, t - s.lastT);
  s.lastT = t;
  s.leftAngle += wL * dt;
  s.rightAngle += wR * dt;
  for (const w of s.wheels.left) w.rotation.z = s.leftAngle;
  for (const w of s.wheels.right) w.rotation.z = s.rightAngle;

  // Camera follows the rover (target slides to its world position).
  api.cam.target.set(x, 0.25, -y);
};
