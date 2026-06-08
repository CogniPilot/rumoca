// Default quadrotor scene for the rumoca sim viewer.
//
// Scene script API:
//   ctx.onInit(api)  — called once after Three.js is ready
//   ctx.onFrame(api) — called each animation frame
//
// api object:
//   api.THREE        — Three.js library
//   api.scene        — THREE.Scene
//   api.renderer     — THREE.WebGLRenderer
//   api.state        — persistent object (store your meshes here)
//   api.get(name)    — read state variable (px, py, pz, q0-q3, omega_m1, etc.)
//   api.motors       — state object (has omega_m1..omega_m4)
//   api.camera       — THREE.Camera
//   api.cam          — { target: THREE.Vector3, dist, angle, elev } (mutable camera orbit)

function controlSignalOn(value) {
  if (value === true || value === 1) return true;
  if (typeof value === "string") {
    const normalized = value.trim().toLowerCase();
    return normalized === "1" || normalized === "true" || normalized === "yes" || normalized === "on";
  }
  return false;
}

function numericSignal(value) {
  const n = Number(value);
  return Number.isFinite(n) ? n : 0;
}

function deterministicUnit(seed) {
  const x = Math.sin(seed * 12.9898) * 43758.5453;
  return x - Math.floor(x);
}

function createCloudTexture(THREE) {
  const canvas = document.createElement("canvas");
  canvas.width = 512;
  canvas.height = 256;
  const ctx = canvas.getContext("2d");
  ctx.clearRect(0, 0, canvas.width, canvas.height);

  const lobes = [
    { x: 140, y: 138, r: 82, a: 0.54 },
    { x: 210, y: 105, r: 98, a: 0.62 },
    { x: 295, y: 118, r: 90, a: 0.58 },
    { x: 365, y: 145, r: 76, a: 0.46 },
    { x: 250, y: 158, r: 126, a: 0.34 },
  ];
  for (const lobe of lobes) {
    const gradient = ctx.createRadialGradient(lobe.x, lobe.y, 0, lobe.x, lobe.y, lobe.r);
    gradient.addColorStop(0, `rgba(255, 255, 255, ${lobe.a})`);
    gradient.addColorStop(0.55, `rgba(245, 247, 244, ${lobe.a * 0.45})`);
    gradient.addColorStop(1, "rgba(255, 255, 255, 0)");
    ctx.fillStyle = gradient;
    ctx.beginPath();
    ctx.arc(lobe.x, lobe.y, lobe.r, 0, Math.PI * 2);
    ctx.fill();
  }

  const texture = new THREE.CanvasTexture(canvas);
  texture.colorSpace = THREE.SRGBColorSpace;
  texture.minFilter = THREE.LinearFilter;
  texture.magFilter = THREE.LinearFilter;
  return texture;
}

function addMarkerLight(THREE, parent, options) {
  const mat = new THREE.MeshStandardMaterial({
    color: options.color,
    emissive: options.color,
    emissiveIntensity: options.emissiveIntensity ?? 1.8,
    roughness: 0.35,
  });
  const geometry = options.shape === "cylinder"
    ? new THREE.CylinderGeometry(
        options.radius ?? 0.018,
        options.radius ?? 0.018,
        options.height ?? 0.018,
        16
      )
    : options.shape === "box"
      ? new THREE.BoxGeometry(
        options.width ?? 0.04,
        options.height ?? 0.014,
        options.depth ?? 0.04
      )
      : new THREE.SphereGeometry(options.radius ?? 0.018, 12, 8);
  const marker = new THREE.Mesh(geometry, mat);
  marker.position.set(options.x, options.y, options.z);
  parent.add(marker);

  const light = new THREE.PointLight(options.color, options.intensity ?? 0.45, options.distance ?? 1.4);
  light.position.copy(marker.position);
  parent.add(light);
  return { marker, mat, light };
}

ctx.onInit = async function(api) {
  const THREE = api.THREE;
  const scene = api.scene;
  const s = api.state;

  // ── Desert skybox ───────────────────────────────────────────────
  const skybox = new THREE.CubeTextureLoader().load([
    "/assets/skybox/arid2_rt.jpg",
    "/assets/skybox/arid2_lf.jpg",
    "/assets/skybox/arid2_up.jpg",
    "/assets/skybox/arid2_dn.jpg",
    "/assets/skybox/arid2_bk.jpg",
    "/assets/skybox/arid2_ft.jpg",
  ]);
  skybox.colorSpace = THREE.SRGBColorSpace;
  scene.background = skybox;
  scene.fog = new THREE.FogExp2(0xc9d4c4, 0.0022);

  const horizonHaze = new THREE.Mesh(
    new THREE.SphereGeometry(460, 64, 32),
    new THREE.ShaderMaterial({
      uniforms: {
        hazeColor: { value: new THREE.Color(0xc9d4c4) },
      },
      vertexShader: `
        varying vec3 vWorldDir;
        void main() {
          vec4 worldPos = modelMatrix * vec4(position, 1.0);
          vWorldDir = normalize(worldPos.xyz - cameraPosition);
          gl_Position = projectionMatrix * viewMatrix * worldPos;
        }
      `,
      fragmentShader: `
        uniform vec3 hazeColor;
        varying vec3 vWorldDir;
        void main() {
          float lowerSky = 1.0 - smoothstep(0.04, 0.30, vWorldDir.y);
          float horizonCore = 1.0 - smoothstep(0.02, 0.16, abs(vWorldDir.y));
          float alpha = max(0.12 * horizonCore, 0.84 * lowerSky);
          if (alpha < 0.01) discard;
          gl_FragColor = vec4(hazeColor, alpha);
        }
      `,
      transparent: true,
      depthWrite: false,
      depthTest: true,
      side: THREE.BackSide,
    })
  );
  horizonHaze.name = "horizon_haze";
  horizonHaze.renderOrder = 900;
  scene.add(horizonHaze);
  s.horizonHaze = horizonHaze;

  const lowerSkyOccluder = new THREE.Mesh(
    new THREE.SphereGeometry(450, 64, 24),
    new THREE.ShaderMaterial({
      uniforms: {
        groundColor: { value: new THREE.Color(0xb99c6c) },
        hazeColor: { value: new THREE.Color(0xc9d4c4) },
      },
      vertexShader: `
        varying vec3 vWorldDir;
        void main() {
          vec4 worldPos = modelMatrix * vec4(position, 1.0);
          vWorldDir = normalize(worldPos.xyz - cameraPosition);
          gl_Position = projectionMatrix * viewMatrix * worldPos;
        }
      `,
      fragmentShader: `
        uniform vec3 groundColor;
        uniform vec3 hazeColor;
        varying vec3 vWorldDir;
        void main() {
          float below = 1.0 - smoothstep(0.02, 0.16, vWorldDir.y);
          if (below < 0.01) discard;
          float deep = smoothstep(0.02, -0.5, vWorldDir.y);
          vec3 color = mix(hazeColor, groundColor, deep);
          gl_FragColor = vec4(color, below);
        }
      `,
      transparent: true,
      depthWrite: false,
      depthTest: true,
      side: THREE.BackSide,
    })
  );
  lowerSkyOccluder.name = "lower_sky_occluder";
  lowerSkyOccluder.renderOrder = 890;
  scene.add(lowerSkyOccluder);
  s.lowerSkyOccluder = lowerSkyOccluder;

  // ── Desert lighting ─────────────────────────────────────────────
  const sun = new THREE.DirectionalLight(0xfff0d0, 1.8);
  sun.position.set(8, 12, 4); scene.add(sun);
  const fill = new THREE.DirectionalLight(0xd4a060, 0.4);
  fill.position.set(-5, 3, -4); scene.add(fill);
  const rim = new THREE.DirectionalLight(0xffeebb, 0.25);
  rim.position.set(0, -1, -6); scene.add(rim);
  scene.add(new THREE.HemisphereLight(0x87ceeb, 0xc2956b, 0.5));

  // ── High clouds ─────────────────────────────────────────────────
  const cloudTexture = createCloudTexture(THREE);
  const cloudGroup = new THREE.Group();
  cloudGroup.name = "cloud_layer";
  scene.add(cloudGroup);
  Array.from({ length: 36 }, (_, i) => {
    const angle = deterministicUnit(i * 11 + 201) * Math.PI * 2;
    const radius = 220 + deterministicUnit(i * 11 + 202) * 760;
    const cloud = new THREE.Sprite(new THREE.SpriteMaterial({
      map: cloudTexture,
      color: 0xffffff,
      transparent: true,
      opacity: 0.22 + deterministicUnit(i * 11 + 203) * 0.26,
      depthWrite: false,
      fog: true,
    }));
    cloud.position.set(
      Math.cos(angle) * radius,
      95 + deterministicUnit(i * 11 + 204) * 75,
      Math.sin(angle) * radius
    );
    const scale = 70 + deterministicUnit(i * 11 + 205) * 130;
    cloud.scale.set(scale * (1.5 + deterministicUnit(i * 11 + 206)), scale, 1);
    cloudGroup.add(cloud);
  });

  // ── Desert ground ───────────────────────────────────────────────
  const textureLoader = new THREE.TextureLoader();
  const configureGroundMap = (texture, color = false) => {
    if (color) texture.colorSpace = THREE.SRGBColorSpace;
    texture.wrapS = THREE.RepeatWrapping;
    texture.wrapT = THREE.RepeatWrapping;
    texture.repeat.set(100, 100);
    texture.minFilter = THREE.LinearMipmapLinearFilter;
    texture.magFilter = THREE.LinearFilter;
    texture.anisotropy = api.renderer?.capabilities?.getMaxAnisotropy?.() ?? 1;
    return texture;
  };
  const sandMat = new THREE.MeshStandardMaterial({
    map: configureGroundMap(textureLoader.load("/assets/sand_pbr/GroundSand005_COL_1K.jpg"), true),
    normalMap: configureGroundMap(textureLoader.load("/assets/sand_pbr/GroundSand005_NRM_1K.jpg")),
    aoMap: configureGroundMap(textureLoader.load("/assets/sand_pbr/GroundSand005_AO_1K.jpg")),
    bumpMap: configureGroundMap(textureLoader.load("/assets/sand_pbr/GroundSand005_BUMP_1K.jpg")),
    color: 0xffffff,
    roughness: 0.82,
    bumpScale: 0.035,
    metalness: 0.02,
  });
  const floor = new THREE.Mesh(new THREE.PlaneGeometry(2000, 2000), sandMat);
  floor.geometry.setAttribute("uv2", new THREE.BufferAttribute(floor.geometry.attributes.uv.array, 2));
  floor.rotation.x = -Math.PI / 2; floor.position.y = -0.01; scene.add(floor);
  const grid = new THREE.GridHelper(2000, 200, 0xe2bc72, 0xb78945);
  grid.material.transparent = true; grid.material.opacity = 0.18;
  scene.add(grid);

  // ── Desert rocks ────────────────────────────────────────────────
  const rockMat = new THREE.MeshStandardMaterial({ color: 0x8b7355, roughness: 0.85, metalness: 0.05 });
  const rockDarkMat = new THREE.MeshStandardMaterial({ color: 0x6b5740, roughness: 0.9, metalness: 0.05 });
  Array.from({ length: 180 }, (_, i) => {
    const angle = deterministicUnit(i * 3 + 1) * Math.PI * 2;
    const radius = 8 + deterministicUnit(i * 3 + 2) ** 0.65 * 420;
    return {
      x: Math.cos(angle) * radius,
      z: Math.sin(angle) * radius,
      s: 0.18 + deterministicUnit(i * 3 + 3) * 0.85,
      squash: 0.35 + deterministicUnit(i * 3 + 4) * 0.35,
      yaw: deterministicUnit(i * 3 + 5) * Math.PI,
      tint: deterministicUnit(i * 3 + 6),
    };
  }).forEach((r) => {
    const rock = new THREE.Mesh(
      new THREE.DodecahedronGeometry(r.s, 1),
      r.tint > 0.45 ? rockMat : rockDarkMat
    );
    rock.position.set(r.x, r.s*0.3, r.z);
    rock.scale.set(1.0 + r.tint * 0.7, r.squash, 0.8 + r.tint * 0.6);
    rock.rotation.set(0.08 + r.tint * 0.18, r.yaw, 0.04 + r.tint * 0.12);
    scene.add(rock);
  });

  // ── Cacti ───────────────────────────────────────────────────────
  const cactusMat = new THREE.MeshStandardMaterial({ color: 0x2f6b35, roughness: 0.82, metalness: 0.02 });
  Array.from({ length: 85 }, (_, i) => {
    const angle = deterministicUnit(i * 7 + 101) * Math.PI * 2;
    const radius = 20 + deterministicUnit(i * 7 + 102) ** 0.7 * 460;
    return {
      x: Math.cos(angle) * radius,
      z: Math.sin(angle) * radius,
      h: 0.7 + deterministicUnit(i * 7 + 103) * 1.6,
      yaw: deterministicUnit(i * 7 + 104) * Math.PI * 2,
      arms: deterministicUnit(i * 7 + 105) > 0.35,
      side: deterministicUnit(i * 7 + 106) > 0.5 ? 1 : -1,
      armScale: 0.55 + deterministicUnit(i * 7 + 107) * 0.6,
    };
  }).forEach((c) => {
    const cactus = new THREE.Group();
    cactus.position.set(c.x, 0, c.z);
    cactus.rotation.y = c.yaw;

    const trunk = new THREE.Mesh(new THREE.CylinderGeometry(0.10, 0.13, c.h, 8), cactusMat);
    trunk.position.y = c.h / 2;
    cactus.add(trunk);
    const top = new THREE.Mesh(new THREE.SphereGeometry(0.105, 8, 6), cactusMat);
    top.position.y = c.h;
    cactus.add(top);

    if (c.arms) {
      const armY = c.h * 0.45;
      const armOut = new THREE.Mesh(new THREE.CylinderGeometry(0.055, 0.07, 0.42 * c.armScale, 7), cactusMat);
      armOut.rotation.z = c.side * Math.PI / 2;
      armOut.position.set(c.side * 0.22 * c.armScale, armY, 0);
      cactus.add(armOut);

      const armUpHeight = 0.38 * c.armScale;
      const armUp = new THREE.Mesh(new THREE.CylinderGeometry(0.05, 0.06, armUpHeight, 7), cactusMat);
      armUp.position.set(c.side * 0.42 * c.armScale, armY + armUpHeight / 2, 0);
      cactus.add(armUp);
      const armTop = new THREE.Mesh(new THREE.SphereGeometry(0.055, 7, 5), cactusMat);
      armTop.position.set(c.side * 0.42 * c.armScale, armY + armUpHeight, 0);
      cactus.add(armTop);
    }

    scene.add(cactus);
  });

  // ── Quadrotor GLB model ─────────────────────────────────────────
  const quad = new THREE.Group();
  quad.name = "quadrotor";
  scene.add(quad);
  s.quad = quad;
  s.propGroups = [];

  const loader = new api.GLTFLoader();
  const gltf = await loader.loadAsync("/assets/models/drone.glb");
  const model = gltf.scene;
  model.name = "drone_glb";

  const bounds = new THREE.Box3().setFromObject(model);
  const center = new THREE.Vector3();
  const size = new THREE.Vector3();
  bounds.getCenter(center);
  bounds.getSize(size);
  const span = Math.max(size.x, size.z, 1e-6);
  const scale = 0.9 / span;
  model.scale.setScalar(scale);
  model.position.set(-center.x * scale, -center.y * scale, -center.z * scale);
  quad.add(model);

  const propNamesByMotor = ["Object_34", "Object_38", "Object_36", "Object_32"];
  const propSpinSignsByMotor = [-1, -1, -1, -1];
  const motorLightColors = [0x28ff45, 0xffffff, 0xffffff, 0xff1d10];
  const propNodes = new Map();
  const motorLightPositions = [];
  model.traverse((object) => {
    if (propNamesByMotor.includes(object.name)) {
      propNodes.set(object.name, object);
    }
    if (object.isMesh) {
      object.castShadow = true;
      object.receiveShadow = true;
    }
  });

  scene.updateMatrixWorld(true);
  for (const propName of propNamesByMotor) {
    const prop = propNodes.get(propName);
    if (!prop) {
      console.warn(`drone.glb missing prop node ${propName}`);
      continue;
    }
    const parent = prop.parent;
    if (!parent) continue;
    const propBounds = new THREE.Box3().setFromObject(prop);
    const propCenter = new THREE.Vector3();
    propBounds.getCenter(propCenter);
    motorLightPositions.push(quad.worldToLocal(propCenter.clone()));
    const pivot = new THREE.Group();
    pivot.name = `${prop.name}_pivot`;
    pivot.position.copy(parent.worldToLocal(propCenter.clone()));
    parent.add(pivot);
    pivot.attach(prop);
    s.propGroups.push({
      group: pivot,
      spinSign: propSpinSignsByMotor[s.propGroups.length] ?? -1,
    });
  }

  if (s.propGroups.length === 0) {
    console.warn("drone.glb did not expose expected prop nodes; prop animation disabled");
  }

  // Port/starboard navigation lights and top arming beacon. The GLB is
  // re-centered and scaled at load time, so lights are anchored from detected
  // motor/prop positions rather than hard-coded arm coordinates.
  motorLightPositions.forEach((pos, i) => {
    const angle = Math.atan2(pos.x, pos.z);
    const outboard = new THREE.Vector3(pos.x, 0, pos.z).normalize().multiplyScalar(0.045);
    addMarkerLight(THREE, quad, {
      x: pos.x + outboard.x,
      y: pos.y - 0.045,
      z: pos.z + outboard.z,
      color: motorLightColors[i] ?? 0xffffff,
      shape: "box",
      width: 0.034,
      height: 0.014,
      depth: 0.018,
      intensity: 0.12,
      distance: 0.38,
      emissiveIntensity: 1.9,
    }).marker.rotation.y = angle;
  });

  const modelWorldBounds = new THREE.Box3().setFromObject(model);
  const bodyTop = quad.worldToLocal(new THREE.Vector3(
    (modelWorldBounds.min.x + modelWorldBounds.max.x) * 0.5,
    modelWorldBounds.max.y,
    (modelWorldBounds.min.z + modelWorldBounds.max.z) * 0.5
  ));
  s.armingBeacon = addMarkerLight(THREE, quad, {
    x: 0,
    y: bodyTop.y + 0.018,
    z: 0,
    color: 0xff2a18,
    shape: "cylinder",
    radius: 0.022,
    height: 0.018,
    intensity: 0,
    distance: 1.8,
    emissiveIntensity: 0.15,
  });

  // ── Ground shadow ───────────────────────────────────────────────
  s.shadow = new THREE.Mesh(
    new THREE.CircleGeometry(0.2, 20),
    new THREE.MeshBasicMaterial({ color: 0x3a2a10, transparent: true, opacity: 0.3 })
  );
  s.shadow.rotation.x = -Math.PI / 2;
  s.shadow.position.y = 0.003;
  scene.add(s.shadow);

  // ── Vehicle-relative camera anchors ─────────────────────────────
  s.vehicleCameraFrame = new THREE.Group();
  s.vehicleCameraFrame.name = "vehicle_camera_frame";
  scene.add(s.vehicleCameraFrame);

  s.cameraTargetAnchor = new THREE.Object3D();
  s.cameraTargetAnchor.name = "camera_target_anchor";
  s.vehicleCameraFrame.add(s.cameraTargetAnchor);

  s.cameraMount = new THREE.Object3D();
  s.cameraMount.name = "camera_mount";
  s.vehicleCameraFrame.add(s.cameraMount);
};

ctx.onFrame = function(api) {
  const THREE = api.THREE;
  const s = api.state;
  const get = api.get;

  const px = get("px") ?? 0;
  const py = get("py") ?? 0;
  const pz = get("pz") ?? 0;
  const q0 = get("q0") ?? 1;
  const q1 = get("q1") ?? 0;
  const q2 = get("q2") ?? 0;
  const q3 = get("q3") ?? 0;

  // Model/world axes are Forward-Left-Up. The mesh is authored in Three.js
  // coordinates with +Z forward, -X left, and +Y up.
  const tx = -py, ty = pz, tz = px;

  // Build DCM from quaternion (body-to-world, NED)
  const R11 = 1-2*(q2*q2+q3*q3), R12 = 2*(q1*q2-q0*q3), R13 = 2*(q1*q3+q0*q2);
  const R21 = 2*(q1*q2+q0*q3), R22 = 1-2*(q1*q1+q3*q3), R23 = 2*(q2*q3-q0*q1);
  const R31 = 2*(q1*q3-q0*q2), R32 = 2*(q2*q3+q0*q1), R33 = 1-2*(q1*q1+q2*q2);

  // Transform: M_three = A * R_model * A^-1, where
  // A maps model FLU vectors to Three vectors: [x3,y3,z3]=[-y,z,x].
  s.quad.matrix.set(
    R22,  -R23, -R21, tx,
   -R32,   R33,  R31, ty,
   -R12,   R13,  R11, tz,
      0,     0,    0,  1
  );
  s.quad.matrixAutoUpdate = false;
  s.quad.matrixWorldNeedsUpdate = true;

  const armed = controlSignalOn(get("armed"));

  // Spin propellers only while armed. Motor state can remain nonzero after a
  // disarm transition, so the visual effect is explicitly gated here.
  const omegas = [
    numericSignal(get("omega_m1")),
    numericSignal(get("omega_m2")),
    numericSignal(get("omega_m3")),
    numericSignal(get("omega_m4")),
  ];
  s.propGroups.forEach((p, i) => {
    if (!armed) return;
    const speed = Math.min(Math.abs(omegas[i]) / 500, 2.0) * 0.8;
    if (speed <= 0) return;
    p.group.rotation.y += p.spinSign * speed;
  });

  if (s.armingBeacon) {
    if (armed) {
      s.armingBeacon.mat.color.setHex(0xff2a18);
      s.armingBeacon.mat.emissive.setHex(0xff2a18);
      s.armingBeacon.mat.emissiveIntensity = 2.4;
      s.armingBeacon.light.color.setHex(0xff2a18);
      s.armingBeacon.light.intensity = 0.75;
    } else {
      const breath = 0.5 + 0.5 * Math.sin(performance.now() * 0.004);
      s.armingBeacon.mat.color.setHex(0xffffff);
      s.armingBeacon.mat.emissive.setHex(0xffffff);
      s.armingBeacon.mat.emissiveIntensity = 0.35 + breath * 0.9;
      s.armingBeacon.light.color.setHex(0xffffff);
      s.armingBeacon.light.intensity = 0.05 + breath * 0.22;
    }
  }

  // Shadow
  s.shadow.position.set(tx, 0.003, tz);
  const alt = Math.max(ty, 0.01);
  const sc = Math.max(0.4, 1.2 - alt * 0.04);
  s.shadow.scale.set(sc, sc, 1);
  s.shadow.material.opacity = Math.max(0.03, 0.25 - alt * 0.015);

  if (api.cameraMode !== "onboard") {
    // Camera follow. Keep the camera anchors translated with the vehicle and
    // rotated by heading only, so roll/pitch maneuvers do not shake the camera.
    const c = api.cam;
    s.vehicleCameraFrame.position.set(tx, ty, tz);
    s.vehicleCameraFrame.rotation.set(0, -Math.atan2(-R12, R22), 0);
    s.cameraTargetAnchor.position.set(0, 0.18, 0);
    s.cameraMount.position.set(
      c.dist * Math.sin(c.angle) * Math.cos(c.elev),
      0.18 + c.dist * Math.sin(c.elev),
      c.dist * Math.cos(c.angle) * Math.cos(c.elev)
    );

    s.vehicleCameraFrame.updateMatrixWorld(true);
    s.cameraTargetAnchor.getWorldPosition(c.target);
    s.cameraMount.getWorldPosition(api.camera.position);
    api.camera.position.y = Math.max(api.camera.position.y, 0.08);
    api.camera.lookAt(c.target);
  }
  if (s.horizonHaze) {
    s.horizonHaze.position.copy(api.camera.position);
  }
  if (s.lowerSkyOccluder) {
    s.lowerSkyOccluder.position.copy(api.camera.position);
  }
};
