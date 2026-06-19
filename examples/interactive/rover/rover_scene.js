// Rover scene for the rumoca sim viewer.
//
// Matches the desert environment used by the quadrotor demo (skybox, haze,
// clouds, PBR sand, rocks, cacti) so a rover and a drone can share the same
// world.
//
// Expects the "chassis" viewer frame from rumoca-scenario.toml to place the
// buggy model in the world. The visual model is loaded from /assets/models.

function deterministicUnit(seed) {
  const x = Math.sin(seed * 12.9898) * 43758.5453;
  return x - Math.floor(x);
}

function createCloudTexture(THREE) {
  const canvas = document.createElement("canvas");
  canvas.width = 512;
  canvas.height = 256;
  const ctx2d = canvas.getContext("2d");
  ctx2d.clearRect(0, 0, canvas.width, canvas.height);

  const lobes = [
    { x: 140, y: 138, r: 82, a: 0.54 },
    { x: 210, y: 105, r: 98, a: 0.62 },
    { x: 295, y: 118, r: 90, a: 0.58 },
    { x: 365, y: 145, r: 76, a: 0.46 },
    { x: 250, y: 158, r: 126, a: 0.34 },
  ];
  for (const lobe of lobes) {
    const gradient = ctx2d.createRadialGradient(lobe.x, lobe.y, 0, lobe.x, lobe.y, lobe.r);
    gradient.addColorStop(0, `rgba(255, 255, 255, ${lobe.a})`);
    gradient.addColorStop(0.55, `rgba(245, 247, 244, ${lobe.a * 0.45})`);
    gradient.addColorStop(1, "rgba(255, 255, 255, 0)");
    ctx2d.fillStyle = gradient;
    ctx2d.beginPath();
    ctx2d.arc(lobe.x, lobe.y, lobe.r, 0, Math.PI * 2);
    ctx2d.fill();
  }

  const texture = new THREE.CanvasTexture(canvas);
  texture.colorSpace = THREE.SRGBColorSpace;
  texture.minFilter = THREE.LinearFilter;
  texture.magFilter = THREE.LinearFilter;
  return texture;
}

ctx.onInit = async function(api) {
  const THREE = api.THREE;
  const scene = api.scene;
  const s = api.state;
  const buggyFrontWheelMeshNames = new Set([
    "group1531753807",
    "group722579550",
  ]);

  function frontWheelCandidates(root) {
    root.updateWorldMatrix(true, true);
    const rootBox = new THREE.Box3().setFromObject(root);
    const rootCenter = new THREE.Vector3();
    const rootSize = new THREE.Vector3();
    rootBox.getCenter(rootCenter);
    rootBox.getSize(rootSize);

    const candidates = [];
    const namedFrontWheels = [];
    root.traverse((object) => {
      if (!object.isMesh || object === root) return;
      const box = new THREE.Box3().setFromObject(object);
      const size = new THREE.Vector3();
      const center = new THREE.Vector3();
      box.getSize(size);
      box.getCenter(center);
      const candidate = { object, center, longitudinal: center.x };
      if (buggyFrontWheelMeshNames.has(object.name)) {
        namedFrontWheels.push(candidate);
      }
      const wheelDiameter = Math.max(size.x, size.y);
      const wheelThickness = Math.min(size.x, size.y, size.z);
      const outboard = Math.abs(center.z - rootCenter.z) > rootSize.z * 0.25;
      const roundish = wheelDiameter > rootSize.x * 0.14 && wheelDiameter < rootSize.x * 0.24;
      const compact = wheelThickness > rootSize.z * 0.06 && wheelThickness < rootSize.z * 0.25;
      if (outboard && roundish && compact) {
        candidates.push(candidate);
      }
    });

    if (namedFrontWheels.length === 2) {
      return namedFrontWheels;
    }

    // Fallback for compatible buggy-like assets: use the remaining central
    // outboard pair rather than either extreme fixed-wheel pair.
    return candidates
      .sort((a, b) => Math.abs(a.longitudinal - rootCenter.x) - Math.abs(b.longitudinal - rootCenter.x))
      .slice(0, 2);
  }

  function createSteeringPivots(root) {
    return frontWheelCandidates(root).map(({ object, center }) => {
      const parent = object.parent;
      const pivot = new THREE.Group();
      pivot.name = `${object.name || "front_wheel"}_steer_pivot`;
      parent.add(pivot);
      pivot.position.copy(parent.worldToLocal(center.clone()));
      pivot.attach(object);
      return pivot;
    });
  }

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

  // ── Desert lighting ───────────────────────────────────────────────
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

  // ── Rover buggy GLB ───────────────────────────────────────────────
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

  const loader = new api.GLTFLoader();
  const gltf = await loader.loadAsync("/assets/models/buggy.glb");
  const buggy = gltf.scene;
  buggy.name = "buggy_glb";
  buggy.traverse((object) => {
    if (object.isMesh) {
      object.castShadow = true;
      object.receiveShadow = true;
    }
  });

  const buggyBounds = new THREE.Box3().setFromObject(buggy);
  const buggyCenter = new THREE.Vector3();
  const buggySize = new THREE.Vector3();
  buggyBounds.getCenter(buggyCenter);
  buggyBounds.getSize(buggySize);
  const buggyFrame = new THREE.Group();
  buggyFrame.name = "buggy_frame";
  buggyFrame.rotation.y = Math.PI / 2;
  buggyFrame.scale.setScalar(1.15 / Math.max(buggySize.x, 1e-6));
  buggy.position.set(-buggyCenter.x, -buggyBounds.min.y, -buggyCenter.z);
  s.frontWheelPivots = createSteeringPivots(buggy);
  buggyFrame.add(buggy);
  rover.add(buggyFrame);

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

  if (!s.rover) return;

  // The chassis pose comes from the viewer's [[viewer.frame]] "chassis"
  // (planar x/y + heading in model FLU coordinates); the viewer owns the
  // FLU-to-renderer conversion, so the configured onboard camera and this
  // placement can never drift apart.
  const chassis = api.frames && api.frames.get("chassis");
  if (!chassis) return;
  s.rover.matrixAutoUpdate = false;
  s.rover.matrix.copy(chassis);
  s.rover.matrixWorldNeedsUpdate = true;
  const steering = Number(get("front_wheel_yaw") ?? get("steering") ?? 0);
  for (const pivot of s.frontWheelPivots || []) {
    pivot.rotation.y = steering;
  }
  if (!s.chassisPos) s.chassisPos = new THREE.Vector3();
  const pos = s.chassisPos.setFromMatrixPosition(chassis);
  s.shadow.position.set(pos.x, 0.001, pos.z);

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
