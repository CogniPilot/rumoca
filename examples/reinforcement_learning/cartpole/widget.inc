<style>
.cartpole-demo{border:1px solid #8c9ba8;border-radius:8px;padding:12px;margin:20px 0}.cartpole-demo canvas{width:100%;height:300px;display:block}.cartpole-demo button{padding:7px 12px;cursor:pointer}.cartpole-demo p{font-size:14px}.cartpole-demo .scores{display:flex;gap:24px;margin:8px 0}.cartpole-demo b{font-size:22px}
</style>
<div class="cartpole-demo">
<button id="cartpole-replay">Replay learned balance</button>
<canvas id="cartpole-scene" aria-label="Recorded cart-pole balancing trajectory"></canvas>
<div class="scores"><span>Zero-force baseline: <b>34 steps</b></span><span>Learned policy: <b>500 steps</b></span></div>
<p id="cartpole-status">Loading recorded native result…</p>
<canvas id="cartpole-learning" style="height:140px" aria-label="Episode score by learning generation"></canvas>
<p>This animation replays the checked native evaluation with learning disabled. The chart shows the actual policy-search history. Use the command below to train a fresh policy.</p>
</div>
<script type="module" src="../repo-examples/reinforcement_learning/cartpole/viewer.js"></script>
