// Three.js scene for the reusable-booster landing example.
// Model coordinates are x downrange, y crossrange, z up. Three.js uses
// {x, y, z} = {model x, model z, -model y}.

const MISSION_PHASE_NAMES = ["READY", "ASCENT", "COAST", "LANDING", "LANDED", "CRASH"];

function finiteSignal(value, fallback = 0) {
  const number = Number(value);
  return Number.isFinite(number) ? number : fallback;
}

function setCylinderBetween(mesh, start, end, delta, localUp) {
  delta.subVectors(end, start);
  mesh.position.copy(start).add(end).multiplyScalar(0.5);
  mesh.scale.set(1, Math.max(delta.length(), 1e-4), 1);
  mesh.quaternion.setFromUnitVectors(localUp, delta.normalize());
}

function makeVehicleMarking(THREE) {
  const canvas = document.createElement("canvas");
  canvas.width = 256;
  canvas.height = 1024;
  const drawing = canvas.getContext("2d");
  drawing.clearRect(0, 0, canvas.width, canvas.height);
  drawing.fillStyle = "#111820";
  drawing.textAlign = "center";
  drawing.textBaseline = "middle";
  drawing.font = "700 54px Arial, sans-serif";
  drawing.fillText("F", 128, 130);
  drawing.fillText("A", 128, 225);
  drawing.fillText("L", 128, 320);
  drawing.fillText("C", 128, 415);
  drawing.fillText("O", 128, 510);
  drawing.fillText("N", 128, 605);
  drawing.font = "700 88px Arial, sans-serif";
  drawing.fillText("9", 128, 760);
  const texture = new THREE.CanvasTexture(canvas);
  texture.colorSpace = THREE.SRGBColorSpace;
  return texture;
}

function makeBoosterSkin(THREE) {
  const canvas = document.createElement("canvas");
  canvas.width = 512;
  canvas.height = 1024;
  const drawing = canvas.getContext("2d");
  drawing.fillStyle = "#eceeeb";
  drawing.fillRect(0, 0, canvas.width, canvas.height);
  const soot = drawing.createLinearGradient(0, canvas.height, 0, 0);
  soot.addColorStop(0, "rgba(22,27,28,0.92)");
  soot.addColorStop(0.24, "rgba(48,53,52,0.78)");
  soot.addColorStop(0.50, "rgba(85,88,84,0.28)");
  soot.addColorStop(0.72, "rgba(105,106,100,0.04)");
  soot.addColorStop(1, "rgba(255,255,255,0)");
  drawing.fillStyle = soot;
  drawing.fillRect(0, 0, canvas.width, canvas.height);
  for (let index = 0; index < 520; index += 1) {
    const x = (index * 137.3) % canvas.width;
    const y = canvas.height - ((index * 73.7) % (canvas.height * 0.72));
    const radius = 1 + ((index * 17) % 11);
    const opacity = 0.025 + 0.07 * (1 - y / canvas.height);
    drawing.fillStyle = `rgba(28,32,31,${Math.max(0.018, opacity)})`;
    drawing.beginPath();
    drawing.ellipse(x, y, radius * 0.45, radius * 2.1, 0.2, 0, Math.PI * 2);
    drawing.fill();
  }
  const texture = new THREE.CanvasTexture(canvas);
  texture.colorSpace = THREE.SRGBColorSpace;
  return texture;
}

function createOcean(THREE) {
  const material = new THREE.ShaderMaterial({
    uniforms: {
      time: { value: 0 },
      deepColor: { value: new THREE.Color(0x0b4463) },
      crestColor: { value: new THREE.Color(0x4f94a8) },
      skyColor: { value: new THREE.Color(0xb9d9e7) },
    },
    vertexShader: `
      uniform float time;
      varying float vWave;
      varying vec3 vWorldPosition;
      void main() {
        vec3 p = position;
        float w1 = sin(p.x * 0.025 + time * 0.9) * 0.55;
        float w2 = sin(p.y * 0.037 - time * 0.7) * 0.34;
        float w3 = sin((p.x + p.y) * 0.014 + time * 0.45) * 0.46;
        p.z += w1 + w2 + w3;
        vWave = (w1 + w2 + w3) / 1.35;
        vec4 world = modelMatrix * vec4(p, 1.0);
        vWorldPosition = world.xyz;
        gl_Position = projectionMatrix * viewMatrix * world;
      }
    `,
    fragmentShader: `
      uniform vec3 deepColor;
      uniform vec3 crestColor;
      uniform vec3 skyColor;
      varying float vWave;
      varying vec3 vWorldPosition;
      void main() {
        float crest = smoothstep(0.15, 0.85, vWave);
        float horizon = smoothstep(300.0, 1200.0, length(vWorldPosition.xz));
        vec3 water = mix(deepColor, crestColor, crest);
        gl_FragColor = vec4(mix(water, skyColor, 0.42 * horizon), 1.0);
      }
    `,
    side: THREE.DoubleSide,
  });
  const ocean = new THREE.Mesh(new THREE.PlaneGeometry(2600, 2600, 180, 180), material);
  ocean.rotation.x = -Math.PI / 2;
  ocean.position.y = -2.2;
  ocean.receiveShadow = true;
  return { ocean, material };
}

function addWindsock(THREE, ship, yellowMaterial) {
  const pole = new THREE.Mesh(
    new THREE.CylinderGeometry(0.12, 0.18, 8.5, 12),
    yellowMaterial,
  );
  pole.position.set(-27, 5.0, -19.0);
  ship.add(pole);

  const root = new THREE.Group();
  root.position.set(-27, 9.2, -19.0);
  ship.add(root);
  const ring = new THREE.Mesh(
    new THREE.TorusGeometry(0.68, 0.07, 8, 20),
    new THREE.MeshStandardMaterial({ color: 0xd9dde0, roughness: 0.45, metalness: 0.7 }),
  );
  ring.rotation.y = Math.PI / 2;
  root.add(ring);

  const orange = new THREE.MeshStandardMaterial({
    color: 0xff6a1a,
    roughness: 0.85,
    side: THREE.DoubleSide,
  });
  const white = new THREE.MeshStandardMaterial({
    color: 0xf4f1e8,
    roughness: 0.9,
    side: THREE.DoubleSide,
  });
  const segments = [];
  const segmentLength = 1.25;
  let parent = root;
  for (let index = 0; index < 6; index += 1) {
    const pivot = new THREE.Group();
    if (index > 0) pivot.position.x = segmentLength;
    parent.add(pivot);
    const radiusStart = 0.62 - index * 0.075;
    const radiusEnd = Math.max(0.12, radiusStart - 0.09);
    const fabric = new THREE.Mesh(
      new THREE.CylinderGeometry(radiusEnd, radiusStart, segmentLength, 18, 1, true),
      index % 2 === 0 ? orange : white,
    );
    fabric.rotation.z = -Math.PI / 2;
    fabric.position.x = segmentLength * 0.5;
    pivot.add(fabric);
    segments.push(pivot);
    parent = pivot;
  }
  return { root, segments };
}

function addDroneShip(THREE, scene) {
  const ship = new THREE.Group();
  ship.name = "autonomous_spaceport_drone_ship";
  scene.add(ship);

  const hullMaterial = new THREE.MeshStandardMaterial({ color: 0x202b31, roughness: 0.72, metalness: 0.4 });
  const deckMaterial = new THREE.MeshStandardMaterial({ color: 0x30383b, roughness: 0.9, metalness: 0.16 });
  const whiteMaterial = new THREE.MeshStandardMaterial({ color: 0xe8ece9, roughness: 0.65 });
  const yellowMaterial = new THREE.MeshStandardMaterial({ color: 0xe6b82e, roughness: 0.68 });

  const hull = new THREE.Mesh(new THREE.BoxGeometry(92, 5.5, 54), hullMaterial);
  hull.position.y = -3.1;
  hull.castShadow = true;
  ship.add(hull);
  const deck = new THREE.Mesh(new THREE.BoxGeometry(88, 0.8, 50), deckMaterial);
  deck.position.y = -0.4;
  deck.receiveShadow = true;
  ship.add(deck);

  const ring = new THREE.Mesh(new THREE.RingGeometry(15.5, 17.2, 96), whiteMaterial);
  ring.rotation.x = -Math.PI / 2;
  ring.position.y = 0.03;
  ship.add(ring);
  for (const angle of [Math.PI / 4, -Math.PI / 4]) {
    const stripe = new THREE.Mesh(new THREE.BoxGeometry(38, 0.08, 1.4), whiteMaterial);
    stripe.rotation.y = angle;
    stripe.position.y = 0.06;
    ship.add(stripe);
  }
  const center = new THREE.Mesh(new THREE.CircleGeometry(1.3, 40), yellowMaterial);
  center.rotation.x = -Math.PI / 2;
  center.position.y = 0.09;
  ship.add(center);
  const innerRing = new THREE.Mesh(new THREE.RingGeometry(10.5, 11.6, 96), yellowMaterial);
  innerRing.rotation.x = -Math.PI / 2;
  innerRing.position.y = 0.08;
  ship.add(innerRing);

  for (const z of [-24.4, 24.4]) {
    const edge = new THREE.Mesh(new THREE.BoxGeometry(88, 0.12, 0.55), yellowMaterial);
    edge.position.set(0, 0.05, z);
    ship.add(edge);
  }
  for (const x of [-43.7, 43.7]) {
    const edge = new THREE.Mesh(new THREE.BoxGeometry(0.55, 0.12, 49), yellowMaterial);
    edge.position.set(x, 0.05, 0);
    ship.add(edge);
  }

  for (const side of [-1, 1]) {
    const wall = new THREE.Mesh(new THREE.BoxGeometry(24, 3.8, 2.4), whiteMaterial);
    wall.position.set(-20, 1.5, side * 23.3);
    wall.castShadow = true;
    ship.add(wall);
    for (let index = 0; index < 4; index += 1) {
      const equipment = new THREE.Mesh(new THREE.BoxGeometry(3.6, 2.2, 2.8), hullMaterial);
      equipment.position.set(-36 + index * 7, 1.2, side * 21.5);
      ship.add(equipment);
    }
  }
  for (const x of [-38, 38]) {
    for (const z of [-22, 22]) {
      const thruster = new THREE.Mesh(new THREE.CylinderGeometry(2.1, 2.6, 5.8, 20), hullMaterial);
      thruster.position.set(x, -4.4, z);
      ship.add(thruster);
    }
  }

  const bridge = new THREE.Mesh(new THREE.BoxGeometry(11, 7, 9), whiteMaterial);
  bridge.position.set(-36, 3.4, 0);
  bridge.castShadow = true;
  ship.add(bridge);
  const bridgeTop = new THREE.Mesh(new THREE.BoxGeometry(7, 1.3, 6), hullMaterial);
  bridgeTop.position.set(-36, 7.5, 0);
  ship.add(bridgeTop);
  const mast = new THREE.Mesh(new THREE.CylinderGeometry(0.24, 0.34, 13, 12), yellowMaterial);
  mast.position.set(-36, 14.2, 0);
  ship.add(mast);

  const windsock = addWindsock(THREE, ship, yellowMaterial);
  const windArrow = new THREE.ArrowHelper(
    new THREE.Vector3(1, 0, 0),
    new THREE.Vector3(24, 0.5, -18),
    11,
    0x65d9e8,
    2.2,
    1.2,
  );
  ship.add(windArrow);

  return { ship, windsock, windArrow };
}

function addGridFin(THREE, booster, angle, material) {
  const fin = new THREE.Group();
  fin.rotation.y = angle;
  booster.add(fin);
  const radialStart = 1.75;
  const radialEnd = 5.1;
  for (let index = 0; index <= 6; index += 1) {
    const bar = new THREE.Mesh(new THREE.BoxGeometry(0.08, 2.8, 0.12), material);
    bar.position.set(radialStart + (radialEnd - radialStart) * index / 6, 15.2, 0);
    fin.add(bar);
  }
  for (let index = 0; index <= 5; index += 1) {
    const bar = new THREE.Mesh(new THREE.BoxGeometry(radialEnd - radialStart, 0.08, 0.12), material);
    bar.position.set((radialStart + radialEnd) * 0.5, 13.8 + 2.8 * index / 5, 0);
    fin.add(bar);
  }
  const hinge = new THREE.Mesh(new THREE.CylinderGeometry(0.17, 0.17, 3.0, 12), material);
  hinge.position.set(radialStart, 15.2, 0);
  fin.add(hinge);
}

function makePlume(THREE, color, opacity) {
  const material = new THREE.MeshBasicMaterial({
    color,
    transparent: true,
    opacity,
    depthWrite: false,
    blending: THREE.AdditiveBlending,
    side: THREE.DoubleSide,
  });
  const plume = new THREE.Mesh(new THREE.ConeGeometry(1, 1, 24, 1, true), material);
  plume.geometry.translate(0, -0.5, 0);
  return plume;
}

function makeExhaustHazeTexture(THREE) {
  const canvas = document.createElement("canvas");
  canvas.width = 96;
  canvas.height = 96;
  const drawing = canvas.getContext("2d");
  const haze = drawing.createRadialGradient(48, 48, 2, 48, 48, 46);
  haze.addColorStop(0, "rgba(255,244,224,0.72)");
  haze.addColorStop(0.28, "rgba(214,221,218,0.34)");
  haze.addColorStop(0.72, "rgba(162,174,176,0.09)");
  haze.addColorStop(1, "rgba(140,152,157,0)");
  drawing.fillStyle = haze;
  drawing.fillRect(0, 0, 96, 96);
  const texture = new THREE.CanvasTexture(canvas);
  texture.colorSpace = THREE.SRGBColorSpace;
  return texture;
}

function makeBoosterMaterials(THREE) {
  const white = new THREE.MeshStandardMaterial({ color: 0xf1f2ef, roughness: 0.52, metalness: 0.18 });
  const black = new THREE.MeshStandardMaterial({ color: 0x11171a, roughness: 0.55, metalness: 0.62 });
  const engineMetal = new THREE.MeshStandardMaterial({ color: 0x343b3e, roughness: 0.38, metalness: 0.85 });
  const bodySkin = new THREE.MeshStandardMaterial({
    map: makeBoosterSkin(THREE),
    roughness: 0.72,
    metalness: 0.08,
  });
  return { white, black, engineMetal, bodySkin };
}

function addBoosterAirframe(THREE, booster, materials) {
  const { white, black, bodySkin } = materials;
  const tank = new THREE.Mesh(new THREE.CylinderGeometry(1.83, 1.83, 35.6, 64), bodySkin);
  tank.position.y = -1.8;
  tank.castShadow = true;
  booster.add(tank);
  const lowerSkirt = new THREE.Mesh(new THREE.CylinderGeometry(1.84, 1.7, 3.7, 64), black);
  lowerSkirt.position.y = -19.65;
  lowerSkirt.castShadow = true;
  booster.add(lowerSkirt);
  const interstage = new THREE.Mesh(new THREE.CylinderGeometry(1.84, 1.84, 5.3, 64), white);
  interstage.position.y = 17.8;
  interstage.castShadow = true;
  booster.add(interstage);
  const topCap = new THREE.Mesh(new THREE.CircleGeometry(1.84, 64), white);
  topCap.rotation.x = -Math.PI / 2;
  topCap.position.y = 20.455;
  booster.add(topCap);

  for (const y of [-16.9, -9.8, 1.2, 10.7, 14.9]) {
    const seam = new THREE.Mesh(new THREE.TorusGeometry(1.835, 0.035, 8, 48), black);
    seam.rotation.x = Math.PI / 2;
    seam.position.y = y;
    booster.add(seam);
  }

  const marking = new THREE.Mesh(
    new THREE.PlaneGeometry(2.8, 13.5),
    new THREE.MeshBasicMaterial({ map: makeVehicleMarking(THREE), transparent: true, depthWrite: false }),
  );
  marking.rotation.y = Math.PI / 2;
  marking.position.set(1.836, 2.8, 0);
  booster.add(marking);

  for (const angle of [0, Math.PI / 2, Math.PI, 3 * Math.PI / 2]) {
    addGridFin(THREE, booster, angle, black);
  }
}

function addEngineCluster(THREE, booster, materials) {
  const { black, engineMetal } = materials;
  const engineMount = new THREE.Mesh(new THREE.CylinderGeometry(1.72, 1.72, 0.8, 32), black);
  engineMount.position.y = -20.0;
  booster.add(engineMount);
  const engines = [];
  let centerNozzle = null;
  for (let index = 0; index < 9; index += 1) {
    const angle = index * Math.PI / 4;
    const radial = index === 8 ? 0 : 1.12;
    const nozzle = new THREE.Mesh(new THREE.CylinderGeometry(0.28, 0.52, 1.55, 20), engineMetal);
    nozzle.position.set(
      index === 8 ? 0 : Math.cos(angle) * radial,
      -20.85,
      index === 8 ? 0 : Math.sin(angle) * radial,
    );
    nozzle.castShadow = true;
    booster.add(nozzle);
    engines.push(nozzle);
    if (index === 8) centerNozzle = nozzle;
  }

  const plumePivot = new THREE.Group();
  plumePivot.position.y = -21.6;
  booster.add(plumePivot);
  const plumeOuter = makePlume(THREE, 0xff6b16, 0.34);
  const plumeHalo = makePlume(THREE, 0xffb14a, 0.16);
  const plumeCore = makePlume(THREE, 0xeafcff, 0.78);
  plumePivot.add(plumeHalo, plumeOuter, plumeCore);
  const shockDiamonds = [];
  for (let index = 0; index < 5; index += 1) {
    const shock = new THREE.Mesh(
      new THREE.OctahedronGeometry(1, 0),
      new THREE.MeshBasicMaterial({
        color: index < 2 ? 0xeafcff : 0xffc46b,
        transparent: true,
        opacity: 0.72,
        depthWrite: false,
        blending: THREE.AdditiveBlending,
      }),
    );
    plumePivot.add(shock);
    shockDiamonds.push(shock);
  }
  const exhaustHaze = [];
  const exhaustHazeTexture = makeExhaustHazeTexture(THREE);
  for (let index = 0; index < 9; index += 1) {
    const haze = new THREE.Sprite(new THREE.SpriteMaterial({
      map: exhaustHazeTexture,
      color: index % 2 === 0 ? 0xffd6a6 : 0xdce8e8,
      transparent: true,
      opacity: 0.08,
      depthWrite: false,
      blending: THREE.AdditiveBlending,
    }));
    plumePivot.add(haze);
    exhaustHaze.push(haze);
  }
  const plumeLight = new THREE.PointLight(0xff7a22, 0, 50, 2);
  plumeLight.position.y = -2;
  plumePivot.add(plumeLight);

  return {
    engines,
    centerNozzle,
    plumePivot,
    plumeHalo,
    plumeOuter,
    plumeCore,
    plumeLight,
    shockDiamonds,
    exhaustHaze,
  };
}

function addLandingLegs(THREE, booster, black) {
  const legs = [];
  const legMaterial = new THREE.MeshStandardMaterial({ color: 0xe9ece9, roughness: 0.58, metalness: 0.45 });
  const footMaterial = new THREE.MeshStandardMaterial({ color: 0x151b1e, roughness: 0.76, metalness: 0.4 });
  for (const angle of [Math.PI / 4, 3 * Math.PI / 4, 5 * Math.PI / 4, 7 * Math.PI / 4]) {
    const direction = new THREE.Vector3(Math.cos(angle), 0, Math.sin(angle));
    const root = direction.clone().multiplyScalar(1.72);
    root.y = -10.4;
    const stowed = direction.clone().multiplyScalar(2.0);
    stowed.y = -17.7;
    const deployed = direction.clone().multiplyScalar(7.8);
    deployed.y = -20.45;
    const braceRoot = direction.clone().multiplyScalar(1.76);
    braceRoot.y = -15.1;
    const main = new THREE.Mesh(new THREE.CylinderGeometry(0.18, 0.28, 1, 14), legMaterial);
    const brace = new THREE.Mesh(new THREE.CylinderGeometry(0.08, 0.12, 1, 12), black);
    const foot = new THREE.Mesh(new THREE.CylinderGeometry(0.72, 0.9, 0.18, 20), footMaterial);
    booster.add(main, brace, foot);
    legs.push({
      root,
      stowed,
      deployed,
      braceRoot,
      footPosition: new THREE.Vector3(),
      braceEnd: new THREE.Vector3(),
      main,
      brace,
      foot,
    });
  }
  return legs;
}

function addRcsJets(THREE, booster, black) {
  const jets = [];
  const addJet = (signal, position, direction) => {
    const pod = new THREE.Mesh(new THREE.BoxGeometry(0.55, 0.75, 0.55), black);
    pod.position.copy(position);
    booster.add(pod);
    const pivot = new THREE.Group();
    pivot.position.copy(position);
    booster.add(pivot);
    const plume = makePlume(THREE, 0xd9f5ff, 0.72);
    plume.scale.set(0.23, 2.4, 0.23);
    plume.quaternion.setFromUnitVectors(new THREE.Vector3(0, -1, 0), direction.clone().normalize());
    pivot.add(plume);
    const light = new THREE.PointLight(0xc8efff, 0, 9, 2);
    pivot.add(light);
    jets.push({ signal, plume, light });
  };
  addJet("rcs_x_positive", new THREE.Vector3(1.92, 16.4, 0), new THREE.Vector3(-1, 0, 0));
  addJet("rcs_x_negative", new THREE.Vector3(-1.92, 16.4, 0), new THREE.Vector3(1, 0, 0));
  addJet("rcs_y_positive", new THREE.Vector3(0, 16.4, -1.92), new THREE.Vector3(0, 0, 1));
  addJet("rcs_y_negative", new THREE.Vector3(0, 16.4, 1.92), new THREE.Vector3(0, 0, -1));
  addJet("rcs_roll_positive", new THREE.Vector3(1.35, 16.8, 1.35), new THREE.Vector3(0, 0, 1));
  addJet("rcs_roll_negative", new THREE.Vector3(-1.35, 16.8, 1.35), new THREE.Vector3(0, 0, 1));
  return jets;
}

function addBooster(THREE, scene) {
  const booster = new THREE.Group();
  booster.name = "reusable_booster_first_stage";
  booster.matrixAutoUpdate = false;
  scene.add(booster);

  const materials = makeBoosterMaterials(THREE);
  addBoosterAirframe(THREE, booster, materials);
  const engineCluster = addEngineCluster(THREE, booster, materials);
  const legs = addLandingLegs(THREE, booster, materials.black);
  const jets = addRcsJets(THREE, booster, materials.black);

  return {
    booster,
    ...engineCluster,
    legs,
    jets,
  };
}

function quinticPosition(time, duration, p0, v0, a0, pf, vf, af) {
  const tau = Math.min(Math.max(time, 0), duration);
  return p0.map((value, axis) => {
    const c0 = value;
    const c1 = v0[axis];
    const c2 = 0.5 * a0[axis];
    const positionDelta = pf[axis] - c0 - c1 * duration - c2 * duration ** 2;
    const velocityDelta = vf[axis] - c1 - 2 * c2 * duration;
    const accelerationDelta = af[axis] - 2 * c2;
    const c3 = 10 * positionDelta / duration ** 3
      - 4 * velocityDelta / duration ** 2
      + 0.5 * accelerationDelta / duration;
    const c4 = -15 * positionDelta / duration ** 4
      + 7 * velocityDelta / duration ** 3
      - accelerationDelta / duration ** 2;
    const c5 = 6 * positionDelta / duration ** 5
      - 3 * velocityDelta / duration ** 4
      + 0.5 * accelerationDelta / duration ** 3;
    return c0 + c1 * tau + c2 * tau ** 2 + c3 * tau ** 3
      + c4 * tau ** 4 + c5 * tau ** 5;
  });
}

function modelPoint(THREE, point) {
  return new THREE.Vector3(point[0], point[2], -point[1]);
}

function makeMarginalVarianceEnvelope(THREE, color) {
  const envelope = new THREE.Group();
  const surface = new THREE.Mesh(
    new THREE.SphereGeometry(1, 20, 14),
    new THREE.MeshBasicMaterial({
      color,
      transparent: true,
      opacity: 0.10,
      wireframe: true,
      depthWrite: false,
      depthTest: false,
    }),
  );
  envelope.add(surface);
  const ringMaterial = new THREE.MeshBasicMaterial({
    color,
    transparent: true,
    opacity: 0.82,
    depthWrite: false,
    depthTest: false,
  });
  for (const rotation of [
    [Math.PI / 2, 0, 0],
    [0, Math.PI / 2, 0],
    [0, 0, 0],
  ]) {
    const ring = new THREE.Mesh(new THREE.TorusGeometry(1, 0.025, 7, 72), ringMaterial);
    ring.rotation.set(rotation[0], rotation[1], rotation[2]);
    envelope.add(ring);
  }
  envelope.renderOrder = 18;
  return envelope;
}

function setThreeSigmaScale(envelope, varianceX, varianceY, varianceZ) {
  const visualGain = 4;
  const sigmaX = visualGain * 3 * Math.sqrt(Math.max(varianceX, 1e-6));
  const sigmaY = visualGain * 3 * Math.sqrt(Math.max(varianceY, 1e-6));
  const sigmaZ = visualGain * 3 * Math.sqrt(Math.max(varianceZ, 1e-6));
  envelope.scale.set(sigmaX, sigmaZ, sigmaY);
}

function addWindIndicator(api) {
  const indicator = document.createElement("div");
  indicator.style.cssText = [
    "position:absolute",
    "top:12px",
    "right:12px",
    "display:flex",
    "align-items:center",
    "gap:8px",
    "padding:7px 9px",
    "border:1px solid rgba(220,240,244,.55)",
    "border-radius:4px",
    "background:rgba(7,24,37,.78)",
    "color:#eef8fa",
    "font:600 12px/1.2 system-ui,sans-serif",
    "pointer-events:none",
  ].join(";");
  const arrow = document.createElement("span");
  arrow.textContent = "->";
  arrow.style.cssText = "display:inline-block;color:#65d9e8;font:700 18px/1 monospace";
  const label = document.createElement("span");
  label.style.whiteSpace = "pre";
  indicator.append(arrow, label);
  api.canvas.parentElement.appendChild(indicator);
  return { indicator, arrow, label };
}

function addTuningPanel(api) {
  const panel = document.createElement("details");
  panel.style.cssText = [
    "position:absolute",
    "top:12px",
    "left:12px",
    "width:min(292px,calc(100% - 24px))",
    "max-height:calc(100% - 72px)",
    "overflow:auto",
    "padding:7px 9px",
    "box-sizing:border-box",
    "border:1px solid rgba(220,240,244,.55)",
    "border-radius:4px",
    "background:rgba(7,24,37,.88)",
    "color:#eef8fa",
    "font:600 12px/1.25 system-ui,sans-serif",
    "pointer-events:auto",
    "z-index:12",
  ].join(";");
  const summary = document.createElement("summary");
  summary.textContent = "Tune environment and controller";
  summary.style.cssText = "cursor:pointer;user-select:none;padding:2px 0";
  panel.appendChild(summary);

  const groups = [
    ["Environment", [
      ["Mean wind", "wind_speed_setting", 0, 15, 0.1, "m/s"],
      ["Wind direction", "wind_direction_setting", -180, 180, 1, "deg"],
      ["Direction swing", "wind_swing_setting", 0, 45, 1, "deg"],
      ["Gust intensity", "gust_setting", 0, 10, 0.1, "m/s"],
      ["Deck heave", "deck_heave_setting", 0, 2, 0.05, "m"],
      ["Deck roll", "deck_roll_setting", 0, 8, 0.1, "deg"],
      ["Deck pitch", "deck_pitch_setting", 0, 8, 0.1, "deg"],
    ]],
    ["Controller gain multipliers", [
      ["Position", "position_gain_setting", 0, 3, 0.05, "x"],
      ["Velocity", "velocity_gain_setting", 0, 3, 0.05, "x"],
      ["Attitude", "attitude_gain_setting", 0, 3, 0.05, "x"],
      ["Angular rate", "rate_gain_setting", 0, 3, 0.05, "x"],
    ]],
  ];
  const controls = [];
  for (const [heading, definitions] of groups) {
    const title = document.createElement("div");
    title.textContent = heading;
    title.style.cssText = "margin:9px 0 5px;color:#77dce8;font-size:11px;text-transform:uppercase";
    panel.appendChild(title);
    for (const [labelText, local, min, max, step, unit] of definitions) {
      const row = document.createElement("label");
      row.style.cssText = "display:grid;grid-template-columns:92px 1fr 58px;gap:6px;align-items:center;margin:5px 0";
      const label = document.createElement("span");
      label.textContent = labelText;
      const slider = document.createElement("input");
      slider.type = "range";
      slider.min = String(min);
      slider.max = String(max);
      slider.step = String(step);
      slider.value = String(finiteSignal(api.getLocal(local)));
      slider.style.cssText = "width:100%;margin:0;accent-color:#65d9e8";
      const value = document.createElement("output");
      value.style.cssText = "text-align:right;font:600 11px monospace;color:#fff";
      const decimals = step < 1 ? (step < 0.1 ? 2 : 1) : 0;
      const showValue = () => {
        value.textContent = `${Number(slider.value).toFixed(decimals)} ${unit}`;
      };
      slider.addEventListener("input", () => {
        api.setLocal(local, Number(slider.value));
        showValue();
      });
      row.append(label, slider, value);
      panel.appendChild(row);
      controls.push({ slider, local, showValue });
      showValue();
    }
  }
  panel.addEventListener("pointerdown", (event) => event.stopPropagation());
  panel.addEventListener("wheel", (event) => event.stopPropagation());
  api.canvas.parentElement.appendChild(panel);
  return { panel, controls };
}

function makeCrashCloudTexture(THREE, innerColor, middleColor) {
  const canvas = document.createElement("canvas");
  canvas.width = 128;
  canvas.height = 128;
  const drawing = canvas.getContext("2d");
  const cloud = drawing.createRadialGradient(64, 64, 3, 64, 64, 62);
  cloud.addColorStop(0, innerColor);
  cloud.addColorStop(0.3, middleColor);
  cloud.addColorStop(0.72, "rgba(45,51,54,.42)");
  cloud.addColorStop(1, "rgba(28,35,38,0)");
  drawing.fillStyle = cloud;
  drawing.fillRect(0, 0, 128, 128);
  const texture = new THREE.CanvasTexture(canvas);
  texture.colorSpace = THREE.SRGBColorSpace;
  return texture;
}

function addCrashEffect(THREE, scene) {
  const group = new THREE.Group();
  group.visible = false;
  scene.add(group);
  const fireTexture = makeCrashCloudTexture(
    THREE,
    "rgba(255,250,214,1)",
    "rgba(255,94,18,.92)",
  );
  const smokeTexture = makeCrashCloudTexture(
    THREE,
    "rgba(116,123,124,.9)",
    "rgba(64,72,74,.72)",
  );
  const particles = [];
  const fireball = new THREE.Sprite(new THREE.SpriteMaterial({
    map: fireTexture,
    color: 0xffffff,
    transparent: true,
    opacity: 0,
    depthWrite: false,
    blending: THREE.AdditiveBlending,
  }));
  group.add(fireball);
  for (let index = 0; index < 32; index += 1) {
    const fire = index < 11;
    const sprite = new THREE.Sprite(new THREE.SpriteMaterial({
      map: fire ? fireTexture : smokeTexture,
      color: 0xffffff,
      transparent: true,
      opacity: 0,
      depthWrite: false,
      blending: fire ? THREE.AdditiveBlending : THREE.NormalBlending,
    }));
    const angle = index * 2.399963;
    const radial = 0.2 + (index % 7) / 7;
    const direction = new THREE.Vector3(
      Math.cos(angle) * radial,
      0.3 + ((index * 11) % 17) / 17,
      Math.sin(angle) * radial,
    );
    group.add(sprite);
    particles.push({
      sprite,
      direction,
      delay: fire ? index * 0.018 : 0.15 + (index - 11) * 0.035,
      baseSize: fire ? 7 + (index % 4) * 2 : 10 + (index % 6) * 2.2,
      fire,
    });
  }
  const light = new THREE.PointLight(0xff7424, 0, 120, 2);
  group.add(light);
  return { group, particles, fireball, light };
}

function rebuildReferencePath(api, values) {
  const THREE = api.THREE;
  const s = api.state;
  if (s.referenceLine) {
    s.referenceLine.geometry.dispose();
    s.referenceLine.parent.remove(s.referenceLine);
  }
  if (s.referenceNodes) s.referenceNodes.clear();
  const points = [];
  for (let index = 0; index <= 160; index += 1) {
    const time = values.duration * index / 160;
    points.push(modelPoint(THREE, quinticPosition(
      time,
      values.duration,
      values.p0,
      values.v0,
      values.a0,
      values.pf,
      values.vf,
      values.af,
    )));
  }
  const material = new THREE.LineBasicMaterial({ color: 0xffc548, transparent: true, opacity: 0.95 });
  s.referenceLine = new THREE.Line(new THREE.BufferGeometry().setFromPoints(points), material);
  s.referenceLine.renderOrder = 20;
  api.scene.add(s.referenceLine);
  for (let index = 0; index <= 10; index += 1) {
    const marker = new THREE.Mesh(
      new THREE.SphereGeometry(index === 10 ? 0.8 : 0.42, 16, 10),
      new THREE.MeshBasicMaterial({ color: index === 10 ? 0xffffff : 0xffc548 }),
    );
    marker.position.copy(points[Math.round(index * 16)]);
    s.referenceNodes.add(marker);
  }
}

ctx.onInit = async function onInit(api) {
  const THREE = api.THREE;
  const scene = api.scene;
  const s = api.state;

  scene.background = new THREE.Color(0xb9d9e7);
  scene.fog = new THREE.FogExp2(0xb9d9e7, 0.0016);
  scene.add(new THREE.HemisphereLight(0xd8eff9, 0x264553, 1.15));
  const sun = new THREE.DirectionalLight(0xfff4dc, 2.4);
  sun.position.set(180, 320, 120);
  sun.castShadow = true;
  sun.shadow.mapSize.set(2048, 2048);
  scene.add(sun);
  const fill = new THREE.DirectionalLight(0x78a8c9, 0.55);
  fill.position.set(-160, 70, -130);
  scene.add(fill);

  const ocean = createOcean(THREE);
  scene.add(ocean.ocean);
  s.oceanMaterial = ocean.material;
  const droneShip = addDroneShip(THREE, scene);
  s.ship = droneShip.ship;
  s.ship.matrixAutoUpdate = false;
  s.windsock = droneShip.windsock;
  s.windArrow = droneShip.windArrow;
  s.windIndicator = addWindIndicator(api);
  s.tuningPanel = addTuningPanel(api);
  Object.assign(s, addBooster(THREE, scene));
  s.crashEffect = addCrashEffect(THREE, scene);
  s.crashActive = false;
  s.crashTime = -1;

  s.referenceNodes = new THREE.Group();
  scene.add(s.referenceNodes);
  s.referenceMarker = new THREE.Mesh(
    new THREE.SphereGeometry(0.48, 20, 12),
    new THREE.MeshBasicMaterial({ color: 0xffd35c, transparent: true, opacity: 0.95 }),
  );
  scene.add(s.referenceMarker);
  s.estimateMarker = new THREE.Mesh(
    new THREE.SphereGeometry(0.20, 16, 10),
    new THREE.MeshBasicMaterial({ color: 0x8cf27c, wireframe: true }),
  );
  scene.add(s.estimateMarker);
  s.gpsMarker = new THREE.Mesh(
    new THREE.OctahedronGeometry(0.24, 0),
    new THREE.MeshBasicMaterial({ color: 0xff72d2, transparent: true, opacity: 0.9 }),
  );
  scene.add(s.gpsMarker);
  s.estimateCovariance = makeMarginalVarianceEnvelope(THREE, 0x8cf27c);
  s.gpsCovariance = makeMarginalVarianceEnvelope(THREE, 0xff72d2);
  scene.add(s.estimateCovariance, s.gpsCovariance);

  s.maxTrailPoints = 1600;
  s.trailPositions = new Float32Array(s.maxTrailPoints * 3);
  s.trailGeometry = new THREE.BufferGeometry();
  s.trailGeometry.setAttribute("position", new THREE.BufferAttribute(s.trailPositions, 3));
  s.trailGeometry.setDrawRange(0, 0);
  s.actualTrail = new THREE.Line(
    s.trailGeometry,
    new THREE.LineBasicMaterial({ color: 0x31e6ff, transparent: true, opacity: 0.92 }),
  );
  scene.add(s.actualTrail);
  s.estimateTrailPositions = new Float32Array(s.maxTrailPoints * 3);
  s.estimateTrailGeometry = new THREE.BufferGeometry();
  s.estimateTrailGeometry.setAttribute(
    "position",
    new THREE.BufferAttribute(s.estimateTrailPositions, 3),
  );
  s.estimateTrailGeometry.setDrawRange(0, 0);
  s.estimateTrail = new THREE.Line(
    s.estimateTrailGeometry,
    new THREE.LineBasicMaterial({ color: 0x8cf27c, transparent: true, opacity: 0.78 }),
  );
  scene.add(s.estimateTrail);
  s.trailCount = 0;
  s.lastTrailTime = -1;
  s.lastGpsUpdate = false;
  s.planSignature = "";
  s.cameraPosition = new THREE.Vector3(105, 68, 112);
  s.cameraTarget = new THREE.Vector3(0, 20, 0);
  s.cameraPan = new THREE.Vector3();
  s.cameraRight = new THREE.Vector3();
  s.cameraUp = new THREE.Vector3();
  s.bodyPosition = new THREE.Vector3();
  s.bodyUp = new THREE.Vector3();
  s.windVisual = new THREE.Vector3();
  s.exhaustDirection = new THREE.Vector3();
  s.localUp = new THREE.Vector3(0, 1, 0);
  s.localDown = new THREE.Vector3(0, -1, 0);
  s.cylinderDelta = new THREE.Vector3();
  s.desiredCameraTarget = new THREE.Vector3();
  s.desiredCameraPosition = new THREE.Vector3();
  s.cameraOffset = new THREE.Vector3();
  s.userCameraActive = false;
  s.lastCameraWallMs = 0;
  api.cam.angle = Math.atan2(90, 110);
  api.cam.elev = 0.38;
  api.cam.dist = 145;
};

function updateVehicleAndCrash(api, bodyFrame, time, crash) {
  const s = api.state;
  s.bodyPosition.setFromMatrixPosition(bodyFrame);
  s.bodyUp.setFromMatrixColumn(bodyFrame, 1);
  s.booster.matrix.copy(bodyFrame);
  s.booster.matrixWorldNeedsUpdate = true;

  if (crash && !s.crashActive) {
    s.crashActive = true;
    s.crashTime = time;
    s.crashEffect.group.position.copy(s.bodyPosition)
      .addScaledVector(s.bodyUp, -20.45);
  } else if (!crash && s.crashActive) {
    s.crashActive = false;
    s.crashTime = -1;
  }
  s.booster.visible = !s.crashActive;
  const crashAge = s.crashActive ? Math.max(0, time - s.crashTime) : -1;
  s.crashEffect.group.visible = crashAge >= 0 && crashAge < 7;
  if (!s.crashEffect.group.visible) return;

  const fireballProgress = Math.min(crashAge / 1.15, 1);
  const fireballSize = 13 + 29 * fireballProgress;
  s.crashEffect.fireball.visible = fireballProgress < 1;
  s.crashEffect.fireball.scale.set(fireballSize, fireballSize, 1);
  s.crashEffect.fireball.material.opacity = Math.max(0, 1 - fireballProgress);
  for (const particle of s.crashEffect.particles) {
    const age = Math.max(0, crashAge - particle.delay);
    const lifetime = particle.fire ? 1.25 : 6.5;
    const progress = Math.min(age / lifetime, 1);
    particle.sprite.visible = age > 0 && progress < 1;
    particle.sprite.position.copy(particle.direction).multiplyScalar(
      particle.fire ? 7 * progress : 4 + 18 * progress,
    );
    particle.sprite.position.y += particle.fire ? 3 * progress : 18 * progress;
    const size = particle.baseSize * (0.5 + 2.2 * progress);
    particle.sprite.scale.set(size, size, 1);
    particle.sprite.material.opacity = particle.fire
      ? Math.max(0, 1 - progress * progress)
      : 0.68 * Math.sin(Math.PI * Math.min(progress, 0.999));
  }
  s.crashEffect.light.intensity = 18 * Math.max(0, 1 - crashAge / 1.1);
}

function updateDeckWindHudAndControls(api, time, crash, missionPhase) {
  const s = api.state;
  const get = api.get;
  const deckPx = finiteSignal(get("deck_px"));
  const deckPy = finiteSignal(get("deck_py"));
  const deckPz = finiteSignal(get("deck_pz"));
  const deckQ0 = finiteSignal(get("deck_q0"), 1);
  const deckQ1 = finiteSignal(get("deck_q1"));
  const deckQ2 = finiteSignal(get("deck_q2"));
  const deckQ3 = finiteSignal(get("deck_q3"));
  const windVx = finiteSignal(get("wind_vx"));
  const windVy = finiteSignal(get("wind_vy"));
  const windSpeed = Math.max(0, finiteSignal(get("wind_speed"), Math.hypot(windVx, windVy)));
  const windDirection = finiteSignal(get("wind_direction"));
  const deckRoll = finiteSignal(get("deck_roll"));
  const deckPitch = finiteSignal(get("deck_pitch"));
  const gustMagnitude = Math.hypot(
    finiteSignal(get("gust_vx")),
    finiteSignal(get("gust_vy")),
    finiteSignal(get("gust_vz")),
  );
  const compactViewer = api.canvas.clientWidth < 640;
  s.windIndicator.indicator.style.top = compactViewer ? "54px" : "12px";
  s.windIndicator.indicator.style.display = compactViewer && s.tuningPanel.panel.open
    ? "none"
    : "flex";

  const dr11 = 1 - 2 * (deckQ2 * deckQ2 + deckQ3 * deckQ3);
  const dr12 = 2 * (deckQ1 * deckQ2 - deckQ0 * deckQ3);
  const dr13 = 2 * (deckQ1 * deckQ3 + deckQ0 * deckQ2);
  const dr21 = 2 * (deckQ1 * deckQ2 + deckQ0 * deckQ3);
  const dr22 = 1 - 2 * (deckQ1 * deckQ1 + deckQ3 * deckQ3);
  const dr23 = 2 * (deckQ2 * deckQ3 - deckQ0 * deckQ1);
  const dr31 = 2 * (deckQ1 * deckQ3 - deckQ0 * deckQ2);
  const dr32 = 2 * (deckQ2 * deckQ3 + deckQ0 * deckQ1);
  const dr33 = 1 - 2 * (deckQ1 * deckQ1 + deckQ2 * deckQ2);
  s.ship.matrix.set(
    dr11, dr13, -dr12, deckPx,
    dr31, dr33, -dr32, deckPz,
    -dr21, -dr23, dr22, -deckPy,
    0, 0, 0, 1,
  );
  s.ship.matrixWorldNeedsUpdate = true;

  const windVisual = s.windVisual.set(windVx, 0, -windVy);
  if (windVisual.lengthSq() > 1e-8) {
    windVisual.normalize();
    s.windArrow.setDirection(windVisual);
    s.windArrow.setLength(6 + Math.min(windSpeed, 30) * 0.55, 2.2, 1.2);
  }
  s.windsock.root.rotation.y = Math.atan2(windVy, windVx);
  const windLift = Math.min(windSpeed / 12, 1);
  for (let index = 0; index < s.windsock.segments.length; index += 1) {
    const segment = s.windsock.segments[index];
    const fraction = (index + 1) / s.windsock.segments.length;
    segment.rotation.z = -(1 - windLift) * 0.22 * fraction
      + Math.sin(time * (5.5 + windSpeed * 0.18) + index * 0.9) * 0.025 * windLift;
    segment.rotation.y = Math.sin(time * 4.1 + index * 0.7) * 0.018 * windLift;
  }
  s.windIndicator.arrow.style.transform = `rotate(${-windDirection.toFixed(1)}deg)`;
  const phaseName = MISSION_PHASE_NAMES[missionPhase] || "UNKNOWN";
  const impactText = crash
    ? ` | ${finiteSignal(get("impact_speed")).toFixed(1)} m/s  ${finiteSignal(get("impact_tilt")).toFixed(0)} deg`
    : "";
  const touchdownText = missionPhase >= 3
    ? `\nCONTACT ${finiteSignal(get("supporting_legs")).toFixed(0)} LEGS  MARGIN ${finiteSignal(get("support_margin")).toFixed(2)} m  N/T ${finiteSignal(get("touchdown_normal_speed")).toFixed(1)}/${finiteSignal(get("touchdown_tangential_speed")).toFixed(1)} m/s`
    : "";
  const feasibilityText = missionPhase === 3 && finiteSignal(get("reference_feasible"), 1) < 0.5
    ? " | REFERENCE INFEASIBLE"
    : "";
  s.windIndicator.label.textContent = `WIND ${windSpeed.toFixed(1)} m/s  ${windDirection.toFixed(0)} deg | GUST ${gustMagnitude.toFixed(1)}\nDECK R ${deckRoll.toFixed(1)} deg  P ${deckPitch.toFixed(1)} deg\n${phaseName}${impactText}${feasibilityText}${touchdownText}`;
  s.windIndicator.indicator.style.borderColor = crash
    ? "rgba(255,91,47,.95)"
    : "rgba(220,240,244,.55)";

  for (const control of s.tuningPanel.controls) {
    if (document.activeElement !== control.slider) {
      const localValue = api.getLocal(control.local);
      if (Number.isFinite(Number(localValue))) {
        control.slider.value = String(localValue);
        control.showValue();
      }
    }
  }
}

function updateActuatorVisuals(api, time) {
  const s = api.state;
  const get = api.get;
  const deployment = Math.min(Math.max(finiteSignal(get("leg_deploy")), 0), 1);
  const smoothDeployment = deployment * deployment * (3 - 2 * deployment);
  for (const leg of s.legs) {
    const footPosition = leg.footPosition.copy(leg.stowed)
      .lerp(leg.deployed, smoothDeployment);
    const braceEnd = leg.braceEnd.copy(leg.root).lerp(footPosition, 0.66);
    setCylinderBetween(leg.main, leg.root, footPosition, s.cylinderDelta, s.localUp);
    setCylinderBetween(leg.brace, leg.braceRoot, braceEnd, s.cylinderDelta, s.localUp);
    leg.foot.position.copy(footPosition);
    leg.foot.rotation.z = smoothDeployment * 0.13;
  }

  const thrust = Math.min(Math.max(finiteSignal(get("thrust")), 0), 1);
  const flicker = 0.91 + 0.09 * Math.sin(time * 71) * Math.sin(time * 37 + 0.4);
  const plumeStrength = thrust * flicker;
  const gimbalX = finiteSignal(get("gimbal_x"));
  const gimbalY = finiteSignal(get("gimbal_y"));
  const exhaustDirection = s.exhaustDirection
    .set(-Math.tan(gimbalX), -1, Math.tan(gimbalY))
    .normalize();
  s.plumePivot.quaternion.setFromUnitVectors(s.localDown, exhaustDirection);
  s.centerNozzle.quaternion.setFromUnitVectors(s.localDown, exhaustDirection);
  s.plumeHalo.visible = thrust > 0.003;
  s.plumeOuter.visible = thrust > 0.003;
  s.plumeCore.visible = thrust > 0.003;
  s.plumeHalo.scale.set(1.2 + 1.6 * plumeStrength, 7 + 30 * plumeStrength, 1.2 + 1.6 * plumeStrength);
  s.plumeOuter.scale.set(0.8 + 1.1 * plumeStrength, 5 + 23 * plumeStrength, 0.8 + 1.1 * plumeStrength);
  s.plumeCore.scale.set(0.34 + 0.5 * plumeStrength, 3 + 14 * plumeStrength, 0.34 + 0.5 * plumeStrength);
  s.plumeHalo.material.opacity = 0.07 + 0.15 * plumeStrength;
  s.plumeOuter.material.opacity = 0.18 + 0.28 * plumeStrength;
  s.plumeCore.material.opacity = 0.42 + 0.5 * plumeStrength;
  s.plumeLight.intensity = 4.5 * plumeStrength;
  for (let index = 0; index < s.shockDiamonds.length; index += 1) {
    const shock = s.shockDiamonds[index];
    const cell = index + 1;
    const cellSpacing = 1.35 + 1.1 * plumeStrength;
    const cellRadius = (0.22 + cell * 0.075) * (0.55 + plumeStrength);
    shock.visible = thrust > 0.04;
    shock.position.set(0, -cell * cellSpacing, 0);
    shock.scale.set(
      cellRadius * (0.9 + 0.1 * Math.sin(time * 83 + index)),
      cellRadius * 1.75,
      cellRadius * (0.9 + 0.1 * Math.cos(time * 77 + index)),
    );
    shock.material.opacity = (0.68 - index * 0.09) * plumeStrength;
  }
  for (let index = 0; index < s.exhaustHaze.length; index += 1) {
    const haze = s.exhaustHaze[index];
    const phase = (time * (7 + 5 * plumeStrength) + index * 3.7) % 32;
    const spread = 0.06 * phase * (0.4 + plumeStrength);
    const hazeScale = 0.45 + phase * 0.075;
    haze.visible = thrust > 0.08;
    haze.position.set(
      Math.sin(index * 2.3 + time * 3.1) * spread,
      -5 - phase,
      Math.cos(index * 1.7 + time * 2.7) * spread,
    );
    haze.scale.set(hazeScale * 0.8, hazeScale * 1.45, hazeScale * 0.8);
    haze.material.opacity = 0.13 * plumeStrength * Math.sin(Math.PI * phase / 32);
  }

  for (const jet of s.jets) {
    const command = Math.min(Math.max(finiteSignal(get(jet.signal)), 0), 1);
    const visibleStrength = Math.pow(command, 0.55) * (0.82 + 0.18 * Math.sin(time * 93));
    jet.plume.visible = command > 0.002;
    jet.plume.scale.y = 1.2 + 3.5 * visibleStrength;
    jet.plume.material.opacity = 0.35 + 0.6 * visibleStrength;
    jet.light.intensity = 2.2 * visibleStrength;
  }
}

function updateGuidanceAndTrails(api, time) {
  const s = api.state;
  const get = api.get;
  const px = finiteSignal(get("px"));
  const py = finiteSignal(get("py"));
  const pz = finiteSignal(get("pz"));
  const tx = s.bodyPosition.x;
  const ty = s.bodyPosition.y;
  const tz = s.bodyPosition.z;
  const p0 = [finiteSignal(get("plan_p0x")), finiteSignal(get("plan_p0y")), finiteSignal(get("plan_p0z"))];
  const v0 = [finiteSignal(get("plan_v0x")), finiteSignal(get("plan_v0y")), finiteSignal(get("plan_v0z"))];
  const a0 = [finiteSignal(get("plan_a0x")), finiteSignal(get("plan_a0y")), finiteSignal(get("plan_a0z"))];
  const pf = [finiteSignal(get("plan_pf_x")), finiteSignal(get("plan_pf_y")), finiteSignal(get("plan_pf_z"))];
  const vf = [finiteSignal(get("plan_vf_x")), finiteSignal(get("plan_vf_y")), finiteSignal(get("plan_vf_z"))];
  const af = [finiteSignal(get("plan_af_x")), finiteSignal(get("plan_af_y")), finiteSignal(get("plan_af_z"))];
  const duration = Math.max(finiteSignal(get("plan_tf"), 22), 0.1);
  const signature = [...p0, ...v0, ...a0, ...pf, ...vf, ...af, duration]
    .map((value) => value.toFixed(4)).join(":");
  if (signature !== s.planSignature) {
    s.planSignature = signature;
    rebuildReferencePath(api, { p0, v0, a0, pf, vf, af, duration });
  }
  s.referenceMarker.position.set(
    finiteSignal(get("ref_px")),
    finiteSignal(get("ref_pz")),
    -finiteSignal(get("ref_py")),
  );
  const estX = finiteSignal(get("est_px"), px);
  const estY = finiteSignal(get("est_py"), py);
  const estZ = finiteSignal(get("est_pz"), pz);
  s.estimateMarker.position.set(estX, estZ, -estY);
  s.estimateCovariance.position.copy(s.estimateMarker.position);
  setThreeSigmaScale(
    s.estimateCovariance,
    finiteSignal(get("est_var_x"), 0.09),
    finiteSignal(get("est_var_y"), 0.09),
    finiteSignal(get("est_var_z"), 0.09),
  );
  const gpsUpdate = finiteSignal(get("gps_update")) > 0.5;
  if (gpsUpdate && !s.lastGpsUpdate) {
    s.gpsMarker.position.set(
      finiteSignal(get("gps_px"), px),
      finiteSignal(get("gps_pz"), pz),
      -finiteSignal(get("gps_py"), py),
    );
    s.gpsCovariance.position.copy(s.gpsMarker.position);
    setThreeSigmaScale(
      s.gpsCovariance,
      finiteSignal(get("gps_var_x"), 0.09),
      finiteSignal(get("gps_var_y"), 0.09),
      finiteSignal(get("gps_var_z"), 0.09),
    );
    s.gpsMarker.scale.setScalar(1.45);
  }
  s.lastGpsUpdate = gpsUpdate;
  s.gpsMarker.scale.multiplyScalar(0.94);
  s.gpsMarker.scale.clampScalar(1.0, 1.45);

  if (time + 0.1 < s.lastTrailTime) {
    s.trailCount = 0;
    s.trailGeometry.setDrawRange(0, 0);
    s.estimateTrailGeometry.setDrawRange(0, 0);
    s.cameraPosition.set(105, 68, 112);
    s.cameraTarget.set(0, 20, 0);
    s.lastCameraWallMs = 0;
  }
  if (time - s.lastTrailTime >= 0.035 && s.trailCount < s.maxTrailPoints) {
    const offset = s.trailCount * 3;
    s.trailPositions[offset] = tx;
    s.trailPositions[offset + 1] = ty;
    s.trailPositions[offset + 2] = tz;
    s.estimateTrailPositions[offset] = estX;
    s.estimateTrailPositions[offset + 1] = estZ;
    s.estimateTrailPositions[offset + 2] = -estY;
    s.trailCount += 1;
    s.trailGeometry.attributes.position.needsUpdate = true;
    s.estimateTrailGeometry.attributes.position.needsUpdate = true;
    s.trailGeometry.setDrawRange(0, s.trailCount);
    s.estimateTrailGeometry.setDrawRange(0, s.trailCount);
    s.lastTrailTime = time;
  }
}

function updateOceanAndCamera(api, time, crash) {
  const s = api.state;
  const get = api.get;
  s.oceanMaterial.uniforms.time.value = time;
  if (api.cameraMode === "scene") {
    const wallMs = finiteSignal(get("wall_ms"));
    const cameraWallDt = s.lastCameraWallMs > 0
      ? Math.min(0.12, Math.max(0.001, (wallMs - s.lastCameraWallMs) / 1000))
      : 1 / 60;
    s.lastCameraWallMs = wallMs;
    const targetBlend = 1 - Math.exp(-3.4 * cameraWallDt);
    const positionBlend = 1 - Math.exp(-2.8 * cameraWallDt);
    const tx = s.bodyPosition.x;
    const ty = s.bodyPosition.y;
    const tz = s.bodyPosition.z;
    const range = Math.hypot(tx, ty);
    const distance = crash ? 86 : Math.max(78, Math.min(270, range * 0.78));
    const desiredTarget = crash
      ? s.desiredCameraTarget.copy(s.crashEffect.group.position)
      : s.desiredCameraTarget.set(tx * 0.48, Math.max(8, ty * 0.5), tz * 0.48);
    const pointer = api.pointer;
    if (pointer.captured) {
      const hasCameraInput = Math.abs(pointer.dx) + Math.abs(pointer.dy)
        + Math.abs(pointer.wheel) > 0.001;
      if ((pointer.buttons & 6) !== 0 && hasCameraInput) {
        s.cameraRight.set(1, 0, 0).applyQuaternion(api.camera.quaternion);
        s.cameraUp.set(0, 1, 0);
        const panScale = api.cam.dist * 0.00055;
        s.cameraPan.addScaledVector(s.cameraRight, -pointer.dx * panScale);
        s.cameraPan.addScaledVector(s.cameraUp, pointer.dy * panScale);
      } else if (hasCameraInput) {
        api.cam.angle -= pointer.dx * 0.004;
        api.cam.elev = Math.min(1.35, Math.max(-0.15, api.cam.elev - pointer.dy * 0.003));
      }
      if (hasCameraInput) {
        api.cam.dist = Math.min(900, Math.max(42, api.cam.dist * Math.exp(pointer.wheel * 0.001)));
        s.userCameraActive = true;
      }
    }
    desiredTarget.add(s.cameraPan);
    const desiredPosition = s.desiredCameraPosition.copy(desiredTarget);
    if (s.userCameraActive) {
      const horizontal = api.cam.dist * Math.cos(api.cam.elev);
      desiredPosition.add(s.cameraOffset.set(
        horizontal * Math.sin(api.cam.angle),
        api.cam.dist * Math.sin(api.cam.elev),
        horizontal * Math.cos(api.cam.angle),
      ));
    } else {
      desiredPosition.add(s.cameraOffset.set(
        distance * 0.78,
        distance * 0.34,
        distance * 0.9,
      ));
    }
    s.cameraTarget.lerp(desiredTarget, targetBlend);
    s.cameraPosition.lerp(desiredPosition, positionBlend);
    api.camera.position.copy(s.cameraPosition);
    api.camera.lookAt(s.cameraTarget);
  }
}

ctx.onFrame = function onFrame(api) {
  const bodyFrame = api.frames && api.frames.get("body");
  if (!bodyFrame) return;
  const time = finiteSignal(api.get("t"));
  const crash = finiteSignal(api.get("crash")) > 0.5;
  const missionPhase = Math.round(finiteSignal(api.get("mission_phase")));

  updateVehicleAndCrash(api, bodyFrame, time, crash);
  updateDeckWindHudAndControls(api, time, crash, missionPhase);
  updateActuatorVisuals(api, time);
  updateGuidanceAndTrails(api, time);
  updateOceanAndCamera(api, time, crash);
};
