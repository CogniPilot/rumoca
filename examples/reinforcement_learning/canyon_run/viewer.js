const vendorCandidates=[new URL('../../vendor/three_viewer.js',document.baseURI),new URL('../../../../packages/rumoca-web/vendor/three_viewer.js',document.baseURI)];
let vendor;
for(const url of vendorCandidates){try{vendor=await import(url.href);break}catch(error){console.debug('Viewer asset unavailable:',url.pathname)}}
if(!vendor)throw Error('Three.js viewer bundle missing; build the local web assets.');
const {THREE,GLTFLoader}=vendor;
import {ctx} from './scene.js';
const host=document.getElementById('scene'),scene=new THREE.Scene();
const renderer=new THREE.WebGLRenderer({antialias:true});
renderer.domElement.className='rumoca-interactive-canvas';
renderer.setPixelRatio(Math.min(devicePixelRatio,2));
renderer.outputColorSpace=THREE.SRGBColorSpace;
renderer.toneMapping=THREE.ACESFilmicToneMapping;
renderer.toneMappingExposure=1.1;host.appendChild(renderer.domElement);
const camera=new THREE.PerspectiveCamera(55,1,.05,3000);
let nativeAvailable=true;
let rows=[],replay=true,replayStart=performance.now(),liveFrame=null;
let frozenNetwork=null,liveNetwork=null,freeCamera=null,watch=false;
let progressHistory=[],lastSteps=0,current=null,returns=[];
let poseFrom=null,poseTarget=null,poseReceivedAt=0,poseDuration=50,lastPoseAt=0;
let lastDrawAt=performance.now(),renderFps=60;
function get(name){
 if(!current)return undefined;
 const values={px:current.position[0],py:current.position[1],pz:current.position[2],
  q0:current.quaternion[0],q1:current.quaternion[1],q2:current.quaternion[2],q3:current.quaternion[3],
  airspeed:current.airspeed,episode:current.episode||1,last_return:current.last_return||0,
  last_distance:current.last_distance||0,epsilon:0,impacts:0,throttle:current.actuators?.[3]||0,
  ail_rad:(current.actuators?.[0]||0)*.5236,elev_rad:(current.actuators?.[1]||0)*.4189,
  rud_rad:(current.actuators?.[2]||0)*.349};
 if(Object.hasOwn(values,name))return values[name];
 const grid=/^occ_(\d+)$/.exec(name);if(grid)return current.observation[18+Number(grid[1])-1];
 const weight=/^w([123])_(\d+)_(\d+)$/.exec(name);
 if(weight){const network=replay?frozenNetwork:liveNetwork;
  return network?.['W'+(Number(weight[1])-1)]?.[Number(weight[3])-1]?.[Number(weight[2])-1]}
 const control=/^control_(\d)$/.exec(name);if(control)return current.applied[Number(control[1])];
 return undefined;
}
THREE.DefaultLoadingManager.setURLModifier(url=>url==='/assets/models/airplane.glb'?new URL('../../assets/models/airplane.glb',import.meta.url).href:url);
const api={THREE,GLTFLoader,scene,renderer,camera,state:{},get,
 cam:{target:new THREE.Vector3(),dist:3.6,angle:Math.PI,elev:.14},episodeReturns:[]};
function rowFrame(r){return {...r,time:r.sim_time,episode:1,last_return:60.767918410944105,last_distance:600.5995984036015}}
async function post(path,body={}){
 const response=await fetch(path,{method:'POST',headers:{'Content-Type':'application/json','X-Demo-Token':window.demoToken},body:JSON.stringify(body)});
 const data=await response.json();if(!response.ok)throw Error(data.error||response.statusText);return data;
}
function graph(){
 const c=document.getElementById('curve'),g=c.getContext('2d');g.clearRect(0,0,c.width,c.height);
 g.strokeStyle='#80939c';g.beginPath();g.moveTo(0,8);g.lineTo(c.width,8);g.stroke();
 g.fillStyle='#78909c';g.font='14px system-ui';g.fillText('600 m finish',12,27);
 g.strokeStyle='#58a989';g.lineWidth=3;g.beginPath();
 progressHistory.forEach((p,i)=>{const x=p.steps/131072*c.width,y=145-Math.min(600,p.last50_mean_distance||0)/600*135;i?g.lineTo(x,y):g.moveTo(x,y)});g.stroke();
}
document.getElementById('replay').onclick=()=>{replay=true;replayStart=performance.now();document.getElementById('label').textContent='Recorded trained flight · learning off'};
document.getElementById('train').onclick=async()=>{try{await post('/api/start');replay=false;progressHistory=[];lastSteps=0;document.getElementById('notice').textContent='Starting a new policy from scratch…'}catch(e){document.getElementById('notice').textContent=e.message}};
document.getElementById('watch').onclick=async()=>{try{watch=!watch;await post('/api/watch',{enabled:watch});replay=false;document.getElementById('watch').textContent=watch?'Train as fast as possible':'Watch training in real time'}catch(e){document.getElementById('notice').textContent=e.message}};
document.getElementById('fullscreen').onclick=()=>{if(document.fullscreenElement)document.exitFullscreen();else host.requestFullscreen()};
window.addEventListener('keydown',e=>{if(e.key.toLowerCase()==='f')document.getElementById('fullscreen').click();if(e.key.toLowerCase()==='t')document.getElementById('watch').click()});
function setCameraPreset(top=false){
 freeCamera={target:new THREE.Vector3(0,10,300),dist:top?720:540,angle:Math.PI,elev:top?Math.PI/2-.01:.48};
 document.getElementById('camera').textContent='Follow camera';
}
function beginFreeCamera(){
 if(freeCamera)return;
 const offset=camera.position.clone().sub(api.cam.target);
 const dist=Math.max(2,offset.length()||api.cam.dist);
 freeCamera={target:api.cam.target.clone(),dist,angle:Math.atan2(offset.x,offset.z),elev:Math.asin(Math.max(-1,Math.min(1,offset.y/dist)))};
 document.getElementById('camera').textContent='Follow camera';
}
function orbitCamera(dx,dy){
 beginFreeCamera();freeCamera.angle-=dx*.006;
 freeCamera.elev=Math.max(-Math.PI/2+.01,Math.min(Math.PI/2-.01,freeCamera.elev+dy*.004));
}
function panCamera(dx,dy){
 beginFreeCamera();freeCamera.target.add(new THREE.Vector3(-dx,dy,0).multiplyScalar(freeCamera.dist/600).applyQuaternion(camera.quaternion));
}
function zoomCamera(ratio){beginFreeCamera();freeCamera.dist=Math.max(2,Math.min(1800,freeCamera.dist/ratio))}
function applyFreeCamera(){
 if(!freeCamera)return;
 const c=freeCamera,rad=c.dist*Math.cos(c.elev);
 camera.position.set(c.target.x+rad*Math.sin(c.angle),c.target.y+c.dist*Math.sin(c.elev),c.target.z+rad*Math.cos(c.angle));
 camera.lookAt(c.target);if(api.state.sky)api.state.sky.position.copy(camera.position);
 renderer.domElement.dataset.viewpoint=JSON.stringify({position:camera.position.toArray(),target:c.target.toArray(),distance:c.dist});
}
document.getElementById('camera').onclick=()=>{
 if(freeCamera){freeCamera=null;document.getElementById('camera').textContent='Overview camera';delete renderer.domElement.dataset.viewpoint}
 else setCameraPreset();
};
document.getElementById('topdown').onclick=()=>setCameraPreset(true);
for(const [id,key] of [['paths','p'],['network','n']])document.getElementById(id).onclick=()=>window.dispatchEvent(new KeyboardEvent('keydown',{key}));
const pointers=new Map(),canvas=renderer.domElement;
canvas.style.touchAction='none';canvas.style.cursor='grab';
function pinch(){const [a,b]=[...pointers.values()];return {distance:Math.hypot(a.x-b.x,a.y-b.y),x:(a.x+b.x)/2,y:(a.y+b.y)/2}}
let previousPinch=null;
canvas.addEventListener('pointerdown',e=>{
 pointers.set(e.pointerId,{x:e.clientX,y:e.clientY,button:e.button});if(pointers.size>=2)previousPinch=pinch();
 canvas.setPointerCapture(e.pointerId);canvas.style.cursor='grabbing';e.preventDefault();
});
canvas.addEventListener('pointermove',e=>{
 const point=pointers.get(e.pointerId);if(!point)return;
 const dx=e.clientX-point.x,dy=e.clientY-point.y;point.x=e.clientX;point.y=e.clientY;
 if(pointers.size>=2){const next=pinch();if(previousPinch?.distance>0&&next.distance>0)zoomCamera(next.distance/previousPinch.distance);if(previousPinch)panCamera(next.x-previousPinch.x,next.y-previousPinch.y);previousPinch=next}
 else if(point.button===0)orbitCamera(dx,dy);else panCamera(dx,dy);
 e.preventDefault();
});
for(const event of ['pointerup','pointercancel','lostpointercapture'])canvas.addEventListener(event,e=>{pointers.delete(e.pointerId);previousPinch=null;if(!pointers.size)canvas.style.cursor='grab'});
canvas.addEventListener('wheel',e=>{zoomCamera(Math.exp(-e.deltaY*.001));e.preventDefault()},{passive:false});
canvas.addEventListener('contextmenu',e=>e.preventDefault());
async function status(){
 try{const response=await fetch('/api/status');if(!response.ok){nativeAvailable=false;document.getElementById('train').disabled=true;document.getElementById('watch').disabled=true;document.getElementById('notice').textContent='Recorded flight. Start the native companion server below to train here.';return}const s=await response.json();if(s.frame && (!liveFrame || s.frame.episode!==liveFrame.episode || s.frame.time!==liveFrame.time)){
   const now=performance.now(),sameEpisode=liveFrame && s.frame.episode===liveFrame.episode && s.frame.time>liveFrame.time;
   poseFrom=sameEpisode&&current?{position:[...current.position],quaternion:[...current.quaternion]}:null;
   poseTarget=s.frame;poseReceivedAt=now;poseDuration=Math.max(16,Math.min(100,now-lastPoseAt));lastPoseAt=now;
  }
  liveFrame=s.frame;returns=s.episodeReturns||[];
  document.getElementById('train').disabled=s.running;
  if(!replay){document.getElementById('label').textContent=s.phase+(s.running?(s.watch?' · real time':' · fast'):'');
   if(s.error)document.getElementById('notice').textContent=s.error;
   else if(s.progress)document.getElementById('notice').textContent=s.running?'Learning from new rollouts':'Training stopped at its recorded budget'}
  if(s.progress){const p=s.progress;document.getElementById('steps').textContent=p.steps.toLocaleString();
   document.getElementById('episodes').textContent=p.episodes;document.getElementById('finishes').textContent=p.finishes;
   document.getElementById('distance').textContent=p.last50_mean_distance.toFixed(1)+' m';
   if(p.steps!==lastSteps){progressHistory.push(p);lastSteps=p.steps;graph()}}
 }catch(e){document.getElementById('notice').textContent='Connection interrupted: '+e.message}
 finally{if(nativeAvailable)setTimeout(status,document.hidden?1000:50)}
}
async function network(){try{if(!nativeAvailable)return;const r=await fetch('/api/network');if(r.ok)liveNetwork=await r.json()}catch(e){document.getElementById('notice').textContent=e.message}finally{if(nativeAvailable)setTimeout(network,1000)}}
async function dataWithFallback(apiPath,file){try{const r=await fetch(apiPath);if(r.ok)return await r.json()}catch(error){console.debug('Using recorded data:',file)}const r=await fetch(new URL(file,import.meta.url));if(!r.ok)throw Error('Missing recorded data: '+file);return await r.json()}
const flight=await dataWithFallback('/api/flight','demo-flight.json');if(flight.error)throw Error(flight.error);
rows=flight.rows;frozenNetwork=await dataWithFallback('/api/network?mode=replay','actor.json');
if(flight.training_curve)progressHistory=flight.training_curve;
if(flight.training_progress){const p=flight.training_progress;document.getElementById('steps').textContent=p.steps.toLocaleString();document.getElementById('episodes').textContent=p.episodes;document.getElementById('finishes').textContent=p.finishes;document.getElementById('distance').textContent=p.last50_mean_distance.toFixed(1)+' m'}

current=rowFrame(rows[0]);await ctx.onInit(api);
document.getElementById('label').textContent='Recorded trained flight · learning off';
replayStart=performance.now();status();network();graph();
function draw(){
 requestAnimationFrame(draw);const now=performance.now(),elapsed=now-lastDrawAt;if(elapsed>0)renderFps=.9*renderFps+.1*1000/elapsed;lastDrawAt=now;const w=host.clientWidth,h=host.clientHeight;
 if(renderer.domElement.width!==Math.round(w*renderer.getPixelRatio())||renderer.domElement.height!==Math.round(h*renderer.getPixelRatio())){
  renderer.setSize(w,h,false);camera.aspect=w/h;camera.updateProjectionMatrix()}
 if(replay){const index=Math.min(rows.length-1,Math.floor((performance.now()-replayStart)/50));current=rowFrame(rows[index]);api.episodeReturns=[60.767918410944105];
  if(index===rows.length-1)document.getElementById('label').textContent='Finish · frozen policy · replay complete';
 }else if(liveFrame){
  current=liveFrame;
  if(watch&&poseFrom&&poseTarget){const alpha=Math.max(0,Math.min(1,(now-poseReceivedAt)/poseDuration));
   const a=new THREE.Quaternion(poseFrom.quaternion[1],poseFrom.quaternion[2],poseFrom.quaternion[3],poseFrom.quaternion[0]);
   const q=poseTarget.quaternion;a.slerp(new THREE.Quaternion(q[1],q[2],q[3],q[0]),alpha);
   current={...liveFrame,position:poseFrom.position.map((x,i)=>x+(poseTarget.position[i]-x)*alpha),quaternion:[a.w,a.x,a.y,a.z]};
  }
  api.episodeReturns=returns;
 }
 if(current){api.replayMode=replay;ctx.onFrame(api);applyFreeCamera();
  document.getElementById('hud').textContent=`${current.time.toFixed(2)} s · ${current.position[0].toFixed(1)} / 600 m\n${replay ? 'Replay playback 1.0×' : (Number.isFinite(current.realtime_rate) ? current.realtime_rate.toFixed(1)+'× real time (rolling)' : 'Measuring simulation rate…')} · ${renderFps.toFixed(0)} render fps\nAltitude ${current.position[2].toFixed(1)} m · ${current.airspeed.toFixed(1)} m/s\nThrust ${current.applied[0].toFixed(2)} · p ${current.applied[1].toFixed(2)} · q ${current.applied[2].toFixed(2)} · r ${current.applied[3].toFixed(2)}`;
 }renderer.render(scene,camera);
}draw();
