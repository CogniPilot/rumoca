"""Serve the guide with bounded native continuous-PPO training."""
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import math
import mimetypes
import re
import os
from pathlib import Path
import secrets
import sys
import threading
import time
import numpy as np

from train import collect, optimize, checkpoint, episode_summary
from ppo import MLP, Adam
from realtime_rate import RealtimeRate
from environment import ContinuousEnv, REPO, configure, sha

HERE = Path(__file__).resolve().parent
TOKEN = secrets.token_hex(24)
LOCK = threading.Lock()
STATE = {'running': False, 'watch': False, 'frame': None, 'progress': None,
         'error': None, 'phase': 'Ready', 'output': None}
RECORDED = json.loads((HERE / 'demo-flight.json').read_text())
FROZEN_NETWORK = json.loads((HERE / 'actor.json').read_text())
STATE['network'] = FROZEN_NETWORK
STATE['progress'] = RECORDED['training_progress']
STATE['episodeReturns'] = [row['return'] for row in json.loads((HERE / 'training-history.json').read_text())]
STOP = threading.Event()
WORKER = None
WASM = None


class VisibleEnv(ContinuousEnv):
    def reset(self):
        self.display_episode = getattr(self, 'display_episode', 0) + 1
        self.display_return = 0.0
        return super().reset()

    def step(self, action):
        if STOP.is_set():
            raise InterruptedError('demo server stopping')
        started = time.monotonic()
        result = super().step(action)
        _, reward, terminated, truncated, info = result
        self.display_return += reward
        if terminated or truncated:
            self.display_last_return = self.display_return
            self.display_last_distance = info['distance']
        frame = {'observation': info['observation'], 'actuators': info['actuators'],
                 'episode': self.display_episode,
                 'last_return': getattr(self, 'display_last_return', 0),
                 'last_distance': getattr(self, 'display_last_distance', 0),
                 'position': info['position'], 'quaternion': self.current_quat.tolist(),
                 'airspeed': info['airspeed'], 'applied': info['applied'],
                 'time': info['sim_time'], 'reward': reward,
                 'finished': info['finished'], 'ended': terminated or truncated}
        with LOCK:
            watch = STATE['watch']
        if watch:
            time.sleep(max(0, self.dt - (time.monotonic() - started)))
        if not hasattr(self, 'display_rate'):
            self.display_rate = RealtimeRate()
        frame['realtime_rate'] = self.display_rate.sample(self.dt, started, time.monotonic())
        with LOCK:
            STATE['frame'] = frame
        return result


def train_visible():
    env, episodes = None, []
    output = None
    result = {'completed': False, 'steps': 0, 'seed': 1, 'height_cost': .10,
              'purpose': 'local guide live native continuous-PPO development'}
    started = time.monotonic()
    try:
        os.sched_setaffinity(0, {CPU})
        stamp = datetime.now(timezone.utc).strftime('%Y%m%dT%H%M%S%fZ')
        output = OUTPUT_ROOT / ('continuous-training-' + stamp)
        output.mkdir(parents=True)
        for name in ['serve.py','train.py','ppo.py','environment.py','stdio.py','realtime_rate.py']:
            __import__('shutil').copy2(HERE/name,output/name)
        with LOCK:
            STATE.update(output=str(output), phase='Starting native Rumoca', error=None)
        rng = np.random.default_rng(1)
        actor, critic = MLP(86, 4, rng, output_scale=.01), MLP(86, 1, rng)
        actor.params['b2'][0] = np.arctanh(-.5)
        with LOCK:
            STATE['network'] = {k: v.tolist() for k, v in actor.params.items()}
        log_std = np.full(4, math.log(.15))
        ao, co = Adam({**actor.params, 'log_std': log_std}), Adam(critic.params)
        env = VisibleEnv(output, WALL, CPU)
        carry = (env.reset(), 0.0, 0)
        checkpoint(output / 'checkpoint-000000.npz', actor, critic, log_std, ao, co, rng, 0)
        with (output / 'episodes.jsonl').open('w') as journal:
            for rollout in range((STEPS+1023)//1024):
                with LOCK:
                    STATE['phase'] = 'Training continuous PPO'
                data, carry, completed = collect(env, actor, critic, log_std, rng,
                                                min(1024,STEPS-rollout*1024), carry, journal, rollout*1024)
                episodes.extend(completed)
                np.savez_compressed(output / f'rollout-{rollout+1:04}.npz', **data)
                metrics = optimize(data, actor, critic, log_std, ao, co, rng, 8, 128)
                total = min(STEPS,(rollout+1)*1024)
                checkpoint(output / f'checkpoint-{total:06}.npz', actor, critic, log_std,
                           ao, co, rng, total)
                progress = {**episode_summary(episodes), 'steps': total, **metrics}
                (output / 'PROGRESS.json').write_text(json.dumps(progress, indent=2)+'\n')
                with LOCK:
                    STATE['progress'] = progress
                    STATE['network'] = {k: v.tolist() for k, v in actor.params.items()}
                    STATE['episodeReturns'] = [r['return'] for r in episodes]
                result['steps'] = total
        result.update(completed=True, **episode_summary(episodes))
        with LOCK:
            STATE['phase'] = 'Training complete'
    except Exception as error:
        result['error'] = repr(error)
        with LOCK:
            STATE.update(error=str(error), phase='Training stopped')
    finally:
        try:
            if env is not None:
                result['native_close'] = env.close()
        except Exception as error:
            result.update(completed=False, cleanup_error=repr(error))
            with LOCK:
                STATE.update(error='Native cleanup incomplete', phase='Training stopped')
        result['elapsed_seconds'] = time.monotonic() - started
        try:
            if output is not None and output.is_dir():
                (output / 'RESULT.json').write_text(json.dumps(result, indent=2)+'\n')
                files = sorted(p for p in output.iterdir() if p.is_file())
                (output / 'SHA256SUMS').write_text(''.join(sha(p)+'  '+p.name+'\n' for p in files))
        except Exception as error:
            with LOCK:
                STATE.update(error=(STATE['error']+'; ' if STATE['error'] else '')+'Result retention failed: '+str(error), phase='Training stopped')
        finally:
            with LOCK:
                STATE['running'] = False


class Handler(BaseHTTPRequestHandler):
    def respond(self, payload, content_type='application/json', status=200):
        data = payload if isinstance(payload, bytes) else json.dumps(payload, allow_nan=False).encode()
        self.send_response(status)
        self.send_header('Content-Type', content_type)
        self.send_header('Content-Length', str(len(data)))
        self.send_header('Cache-Control', 'no-store')
        self.end_headers()
        if self.command != 'HEAD':
            self.wfile.write(data)

    def do_HEAD(self):
        self.do_GET()

    def do_GET(self):
        route = self.path.split('?', 1)[0]
        if route in ['/', '/user-guide/simulation/canyon-run.html']:
            self.respond((BOOK/'simulation/canyon-run.html').read_text().replace('__TOKEN__', TOKEN).encode(), 'text/html; charset=utf-8')
        elif route.startswith('/vendor/') or route.startswith('/pkg/'):
            root = VENDOR if route.startswith('/vendor/') else WASM
            prefix = '/vendor/' if route.startswith('/vendor/') else '/pkg/'
            path = (root / route[len(prefix):]).resolve() if root else None
            if path and path.is_relative_to(root.resolve()) and path.is_file():
                self.respond(path.read_bytes(), mimetypes.guess_type(path.name)[0] or 'application/octet-stream')
            else:
                self.respond({'error': 'not found'}, status=404)
        elif route == '/api/network':
            with LOCK:
                network = FROZEN_NETWORK if 'mode=replay' in self.path else STATE['network']
            self.respond(network)
        elif route == '/api/status':
            with LOCK:
                state = {k:v for k,v in STATE.items() if k != 'network'}
            self.respond(state)
        elif route == '/api/flight':
            self.respond(RECORDED)
        elif route.startswith('/user-guide/'):
            path = (BOOK / route[len('/user-guide/'):]).resolve()
            if path.is_relative_to(BOOK.resolve()) and path.is_file():
                self.respond(path.read_bytes(), mimetypes.guess_type(path.name)[0] or 'application/octet-stream')
            else:
                self.respond({'error':'not found'}, status=404)
        else:
            self.respond({'error': 'not found'}, status=404)

    def do_POST(self):
        if self.headers.get('X-Demo-Token') != TOKEN:
            self.respond({'error': 'invalid demo request'}, status=403)
            return
        length = int(self.headers.get('Content-Length', '0'))
        if not 0 <= length <= 512:
            self.respond({'error': 'oversized request'}, status=413)
            return
        try:
            payload = json.loads(self.rfile.read(length) or b'{}')
        except (ValueError, TypeError):
            self.respond({'error': 'invalid request'}, status=400)
            return
        if self.path == '/api/start':
            global WORKER
            with LOCK:
                if STATE['running']:
                    self.respond({'error': 'training is already running'}, status=409)
                    return
                STATE.update(running=True, progress=None, frame=None, watch=False, error=None, phase='Starting native Rumoca')
            WORKER = threading.Thread(target=train_visible)
            try:
                WORKER.start()
            except Exception as error:
                with LOCK:
                    STATE.update(running=False, error=str(error), phase='Training stopped')
                WORKER = None
                self.respond({'error':'worker could not start'}, status=500)
                return
        elif self.path == '/api/watch' and type(payload.get('enabled')) is bool:
            with LOCK:
                STATE['watch'] = payload['enabled']
        else:
            self.respond({'error': 'unsupported request'}, status=400)
            return
        self.respond({'ok': True})

    def log_message(self, *args):
        pass


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--port', type=int, default=8080)
    parser.add_argument('--bind', default='127.0.0.1')
    parser.add_argument('--rumoca', default='rumoca')
    parser.add_argument('--source-root', type=Path, default=REPO/'target/cmm/CMM-a642c381')
    parser.add_argument('--guide-root', type=Path, default=REPO/'docs/user-guide/book')
    parser.add_argument('--vendor-root', type=Path, default=REPO/'packages/rumoca-web/vendor')
    parser.add_argument('--wasm-root', type=Path, default=REPO/'packages/rumoca/dist')
    parser.add_argument('--output-root', type=Path, default=Path('canyon-runs'))
    parser.add_argument('--cpu', type=int, default=min(os.sched_getaffinity(0)))
    parser.add_argument('--steps', type=int, default=131072)
    parser.add_argument('--wall', type=int, default=900)
    args = parser.parse_args()
    if min(args.steps,args.wall) < 1:
        parser.error('steps and wall must be positive')
    configure(args.rumoca,args.source_root)
    BOOK,VENDOR,OUTPUT_ROOT = args.guide_root.resolve(),args.vendor_root.resolve(),args.output_root.resolve()
    WASM = args.wasm_root.resolve()
    CPU,STEPS,WALL = args.cpu,args.steps,args.wall
    if not (BOOK/'simulation/canyon-run.html').is_file() or not (VENDOR/'three_viewer.js').is_file():
        parser.error('built guide and Three.js vendor bundle are required; see the guide setup commands')
    try:
        with ThreadingHTTPServer((args.bind,args.port),Handler) as server:
            server.serve_forever()
    finally:
        STOP.set()
        if WORKER is not None and WORKER.ident is not None:
            WORKER.join(timeout=10)
