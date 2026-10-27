import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
const g=new Proxy({}, {get:(target,name)=>target[name]??(()=>{})});
const canvas=()=>({clientWidth:320,clientHeight:150,width:0,height:0,getContext:()=>g});
const context={ctx:{},devicePixelRatio:1};
vm.runInNewContext(fs.readFileSync(new URL('./cartpole_scene.js',import.meta.url),'utf8')+';globalThis.update=updateLearningPanels;',context);
const state={network:canvas(),learning:canvas(),scores:[],lastEpisode:0};
let values={episode:1,t:0,last:0,w1:1,w2:-2,w3:3,w4:4};
const api={state,get:name=>values[name]};
function frame(episode,time,last) {Object.assign(values,{episode,t:time,last});context.update(api)}
function expected(rows) {assert.equal(JSON.stringify(state.scores),JSON.stringify(rows))}
frame(1,0,0);expected([]);
frame(2,.7,35);expected([{episode:1,score:35}]);
frame(5,8,99);expected([{episode:1,score:35},{episode:4,score:99}]);
frame(5,8.1,99);expected([{episode:1,score:35},{episode:4,score:99}]);
frame(1,0,0);expected([]);
frame(2,.3,12);expected([{episode:1,score:12}]);
frame(2,0,0);expected([]);
frame(3,1,NaN);expected([]);
console.log('8 actual-function history controls PASS: normal/sparse/duplicate/reset/new-run/time-regression/nonfinite');
