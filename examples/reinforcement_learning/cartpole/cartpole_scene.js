// The scene only reads the Modelica learner. It never chooses policy weights.
// A bump is a signed 20 N, 0.3 simulated-second force pulse, after training.
ctx.onInit = function(api) {
  const {THREE, scene, state, canvas} = api;
  scene.background = new THREE.Color(0x0a1520);
  scene.add(new THREE.HemisphereLight(0xd5e9ff, 0x173047, 2.8));
  const light = new THREE.DirectionalLight(0xffffff, 2);
  light.position.set(2, 6, 4);
  scene.add(light);
  api.camera.position.set(0, 1.3, 6.5);
  api.camera.lookAt(0, 0.6, 0);
  api.cam.target.set(0,1.85,0);
  Object.assign(api.cam, {angle:Math.PI/2, elev:0.08, dist:5.3});

  function box(w,h,d,color) {
    return new THREE.Mesh(new THREE.BoxGeometry(w,h,d),
      new THREE.MeshStandardMaterial({color, roughness:0.45}));
  }
  const rail = box(5.3, 0.04, 0.12, 0x8495a4);
  rail.position.y=1.15;
  scene.add(rail);
  for(const x of [-2.5,2.5]) {
    const post=box(0.07,1.15,0.18,0x435c70);
    post.position.set(x,0.575,0);
    scene.add(post);
  }
  for (const x of [-2,2]) {
    const limit = box(0.025,0.24,0.3,0xc9755d);
    limit.position.set(x,1.25,0);
    scene.add(limit);
  }
  state.cart = new THREE.Group();
  state.cart.position.y=1.15;
  const body = box(0.42,0.19,0.28,0x357b9b);
  body.position.y=0.15;
  state.cart.add(body);
  for(const x of [-0.14,0.14]) {
    const wheel=new THREE.Mesh(new THREE.CylinderGeometry(0.06,0.06,0.32,20),
      new THREE.MeshStandardMaterial({color:0x344752}));
    wheel.rotation.x=Math.PI/2;
    wheel.position.set(x,0.06,0);
    state.cart.add(wheel);
  }
  state.pole = new THREE.Group();
  state.pole.position.y=0.23;
  const rod=box(0.045,1,0.045,0xdba64c);
  rod.position.y=0.5;
  state.pole.add(rod);
  const tip=new THREE.Mesh(new THREE.SphereGeometry(0.075,20,16),
    new THREE.MeshStandardMaterial({color:0xe7b65c,metalness:0.2,roughness:0.35}));
  tip.position.y=1;
  state.pole.add(tip);
  state.cart.add(state.pole);
  scene.add(state.cart);

  const doc=canvas.ownerDocument, panel=doc.createElement('div');
  panel.className='rumoca-interactive-controls';
  panel.style.cssText='all:unset;display:block;position:absolute;left:16px;right:16px;top:16px;padding:12px 16px;border-radius:10px;background:#102434ee;color:#d5e9ef;font:14px system-ui;box-shadow:0 2px 14px #2342;z-index:3';
  state.status=doc.createElement('div');
  state.status.style.cssText='font-weight:650;margin-bottom:6px';
  state.detail=doc.createElement('div');
  state.detail.style.marginBottom='8px';
  state.buttons=[];
  state.bumpSequence=0;
  for(const [label,direction] of [['← Bump left (20 N)',-1],['Bump right (20 N) →',1]]) {
    const button=doc.createElement('button');
    button.textContent=label;
    button.disabled=true;
    button.style.cssText='padding:7px 12px;margin-right:8px;border:1px solid #88a5b5;border-radius:6px;background:#1c3d52;color:#d5e9ef;cursor:pointer';
    button.addEventListener('pointerdown',event=>event.stopPropagation());
    button.onclick=()=>{
      if(!Boolean(api.get('trained')) || Number(api.get('bump'))!==0) return;
      // Every request has a new value, including repeated bumps in one direction.
      api.setLocal('bump',direction*(++state.bumpSequence));
    };
    state.buttons.push(button);
  }
  panel.append(state.status,state.detail,...state.buttons);
  const resetView=doc.createElement('button');
  resetView.textContent='Reset view';
  resetView.style.cssText=state.buttons[0].style.cssText;
  resetView.onclick=()=>homeView(api);
  panel.append(resetView);
  const hint=doc.createElement('div');
  hint.textContent='Drag to rotate · right drag to pan · pinch/scroll to zoom';
  hint.style.cssText='font-size:12px;opacity:.8;margin-top:8px';
  panel.append(hint);
  canvas.parentElement.append(panel);
  installOrbit(api);
  state.scores=[];
  state.lastEpisode=0;
  const analytics=doc.createElement('div');
  analytics.style.cssText='display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:12px;padding:12px;background:#0a1520;color:#d5e9ef;border-radius:0 0 8px 8px';
  for(const name of ['network','learning']) {
    const graph=doc.createElement('canvas');
    graph.style.cssText='width:100%;height:150px;display:block';
    graph.setAttribute('aria-label',name==='network'?'Actual neural policy weights':'Observed episode scores');
    analytics.append(graph);
    state[name]=graph;
  }
  (canvas.closest('.rumoca-live-interactive')||canvas.parentElement.parentElement).append(analytics);
};

ctx.onFrame = function(api) {
  const {state,get}=api;
  const time=Number(get('t'))||0, trained=Boolean(get('trained'));
  state.cart.position.x=Number(get('x'))||0;
  state.pole.rotation.z=-(Number(get('theta'))||0);
  state.buttons.forEach(button=>button.disabled=!trained || Number(get('bump'))!==0);
  state.status.textContent=trained?'Policy frozen · try a 0.3 s bump':'Learning in Modelica · Realtime / Fast controls below';
  updateCapturedOrbit(api);
  updateLearningPanels(api);
  state.detail.textContent=`Episode ${get('episode')} · best ${Number.isFinite(get('best'))?get('best')+' / 500 steps':(Number(get('bestScore'))>-1e8?Number(get('bestScore')).toFixed(2)+' reward':'awaiting first episode')} · t=${time.toFixed(2)} s · x=${Number(get('x')).toFixed(2)} m · bump=${Number(get('bump')).toFixed(0)} N · rail hits=${Number(get('railHits'))||0} · tilt=${(Number(get('theta'))*180/Math.PI).toFixed(1)}°`;
};


function plotSurface(canvas) {
  const ratio=Math.min(devicePixelRatio,2);
  const width=Math.max(1,canvas.clientWidth),height=canvas.clientHeight;
  if(canvas.width!==Math.round(width*ratio)||canvas.height!==Math.round(height*ratio)) {
    canvas.width=Math.round(width*ratio);canvas.height=Math.round(height*ratio);
  }
  const g=canvas.getContext('2d');g.setTransform(ratio,0,0,ratio,0,0);
  g.clearRect(0,0,width,height);g.fillStyle='#bfd7e6';g.font='12px system-ui';
  return {g,width,height};
}

function updateLearningPanels(api) {
  const {state,get}=api;
  const weights=[];
  for(let i=1;i<=57;i++) {
    const value=get('w'+i);
    if(!Number.isFinite(value))break;
    weights.push(value);
  }
  const {g,width,height}=plotSurface(state.network);
  g.fillText(weights.length===7?'7 physics features → 1 · learned gains':weights.length===57?'5 → 8 → 1 · actual neural weights':'4 → 1 · actual neural weights',8,16);
  if(weights.length===4) {
    ['position','velocity','angle','angular rate'].forEach((label,i)=>{
      const y=40+i*29,x=width-32;
      g.strokeStyle=weights[i]>=0?'#4ec5df':'#e2a957';
      g.lineWidth=1+Math.min(4,Math.abs(weights[i]));
      g.beginPath();g.moveTo(95,y);g.lineTo(x,85);g.stroke();
      g.fillStyle='#d5e9ef';g.fillText(label,8,y+4);
      g.fillText(weights[i].toFixed(2),108,y+3);
    });
  } else if(weights.length===7) {
    const labels=['swing position','swing velocity','energy pumping','balance position',
      'balance velocity','balance angle','balance rate'];
    labels.forEach((label,i)=>{
      const y=34+i*16;
      g.strokeStyle=weights[i]>=0?'#4ec5df':'#e2a957';
      g.lineWidth=1+Math.min(3,Math.abs(weights[i]));
      g.beginPath();g.moveTo(138,y);g.lineTo(width-15,83);g.stroke();
      g.fillStyle='#d5e9ef';g.fillText(label,8,y+3);
      g.fillText(weights[i].toFixed(2),142,y+3);
    });
  } else if(weights.length===57) {
    const cell=Math.min(28,(width-30)/8);
    // Each column is a hidden unit: five inputs plus its bias.
    for(let unit=0;unit<8;unit++)for(let input=0;input<6;input++) {
      const value=weights[unit*6+input];
      g.globalAlpha=0.18+0.82*Math.tanh(Math.abs(value));
      g.fillStyle=value>=0?'#4ec5df':'#e2a957';
      g.fillRect(10+unit*cell,30+input*16,cell-2,14);
    }
    g.globalAlpha=1;g.fillStyle='#bfd7e6';
    g.fillText('cyan + / gold − · intensity = weight size',8,height-7);
  }
  const episode=Number(get('episode'));
  const time=Number(get('t'));
  if(episode<state.lastEpisode || time<state.lastPlotTime) {
    state.scores=[];
  } else if(episode>state.lastEpisode && state.lastEpisode>0) {
    const score=Number.isFinite(get('lastScore'))?get('lastScore'):get('last');
    if(Number.isFinite(score))state.scores.push({episode:episode-1,score});
    if(state.scores.length>256)state.scores.shift();
  }
  state.lastEpisode=episode;
  state.lastPlotTime=time;
  const chart=plotSurface(state.learning),c=chart.g;
  c.fillText('Observed episode scores · higher is better',8,16);
  const max=Math.max(1,...state.scores.map(row=>row.score));
  const min=Math.min(0,...state.scores.map(row=>row.score));
  c.strokeStyle='#58c5a1';c.lineWidth=2;c.beginPath();
  state.scores.forEach((row,i)=>{
    const x=10+i/Math.max(1,state.scores.length-1)*(chart.width-20);
    const y=chart.height-15-(row.score-min)/(max-min)*(chart.height-45);
    i?c.lineTo(x,y):c.moveTo(x,y);
  });c.stroke();
  c.fillStyle='#8ca9b9';
  c.fillText('Fast display may skip episodes.',8,chart.height-1);
}


function applyOrbitView(api,render=true) {
  const {cam,camera,canvas}=api;
  const radius=cam.dist*Math.cos(cam.elev);
  camera.position.set(cam.target.x+radius*Math.cos(cam.angle),
    cam.target.y+cam.dist*Math.sin(cam.elev),cam.target.z+radius*Math.sin(cam.angle));
  camera.lookAt(cam.target);
  canvas.dataset.viewpoint=JSON.stringify({position:camera.position.toArray(),distance:cam.dist});
  if(render)api.renderer.render(api.scene,camera);
}
function homeView(api) {
  api.cam.target.set(0,1.85,0);
  Object.assign(api.cam,{angle:Math.PI/2,elev:0.12,dist:5.3});
  applyOrbitView(api);
}
function orbitView(api,dx,dy) {
  if(!Number.isFinite(dx)||!Number.isFinite(dy))return;
  api.cam.angle-=dx*0.006;
  api.cam.elev=Math.max(-1.15,Math.min(1.15,api.cam.elev+dy*0.004));
  applyOrbitView(api);
}
function panView(api,dx,dy) {
  const delta=new api.THREE.Vector3(-dx,dy,0)
    .multiplyScalar(api.cam.dist/600).applyQuaternion(api.camera.quaternion);
  api.cam.target.add(delta);
  applyOrbitView(api);
}
function zoomView(api,ratio) {
  if(!Number.isFinite(ratio)||ratio<=0)return;
  api.cam.dist=Math.max(2.5,Math.min(15,api.cam.dist/ratio));
  applyOrbitView(api);
}
function pinchGeometry(pointers) {
  const [a,b]=[...pointers.values()];
  return {distance:Math.hypot(a.x-b.x,a.y-b.y),x:(a.x+b.x)/2,y:(a.y+b.y)/2};
}
function installOrbit(api) {
  const {canvas,state}=api;
  const pointers=new Map();
  canvas.style.touchAction='none';canvas.style.cursor='grab';
  canvas.setAttribute('aria-label','3D cart-pole: drag to rotate, pinch or scroll to zoom');
  homeView(api);
  canvas.addEventListener('pointerdown',event=>{
    pointers.set(event.pointerId,{x:event.clientX,y:event.clientY,button:event.button});
    if(pointers.size>=2)state.pinch=pinchGeometry(pointers);
    try{canvas.setPointerCapture(event.pointerId)}catch(error){/* Optional on synthetic events. */}
    canvas.style.cursor='grabbing';event.preventDefault();
  });
  canvas.addEventListener('pointermove',event=>{
    const point=pointers.get(event.pointerId);if(!point)return;
    const dx=event.clientX-point.x,dy=event.clientY-point.y;
    point.x=event.clientX;point.y=event.clientY;
    if(pointers.size>=2) {
      const next=pinchGeometry(pointers),previous=state.pinch;
      if(previous?.distance>0&&next.distance>0)zoomView(api,next.distance/previous.distance);
      if(previous)panView(api,next.x-previous.x,next.y-previous.y);
      state.pinch=next;
    } else if(point.button===0)orbitView(api,dx,dy);
    else panView(api,dx,dy);
    event.preventDefault();
  });
  const end=event=>{
    pointers.delete(event.pointerId);state.pinch=null;
    if(!pointers.size)canvas.style.cursor='grab';
  };
  for(const event of ['pointerup','pointercancel','lostpointercapture'])canvas.addEventListener(event,end);
  canvas.addEventListener('wheel',event=>{
    zoomView(api,Math.exp(-event.deltaY*0.001));event.preventDefault();
  },{passive:false});
  canvas.addEventListener('contextmenu',event=>event.preventDefault());
}
function updateCapturedOrbit(api) {
  const pointer=api.pointer;
  if(pointer?.captured) {
    if(pointer.buttons&6)panView(api,pointer.dx,pointer.dy);
    else orbitView(api,pointer.dx,pointer.dy);
    if(pointer.wheel)zoomView(api,Math.exp(-pointer.wheel*0.001));
  }
  applyOrbitView(api,false);
}
