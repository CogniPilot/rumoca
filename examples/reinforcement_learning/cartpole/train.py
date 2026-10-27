"""Learn four CartPole feedback gains from episode reward, using only stdlib."""
import argparse
import csv
import json
import math
import os
from pathlib import Path
import random
import selectors
import shutil
import subprocess
import tempfile
import time


class Session:
    def __init__(self, binary, scratch, deadline):
        self.deadline = deadline
        self.time = 0.0
        self.buffer = b''
        self.log = (scratch / 'stderr.log').open('wb')
        self.child = subprocess.Popen([binary, 'sim', str(Path(__file__).with_name('CartPole.mo')),
            '-m', 'CartPole', '--solver', 'rk-like', '--serve-stdio',
            '--cache-dir', str(scratch / 'cache')], stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=self.log)
        self.selector = selectors.DefaultSelector()
        self.selector.register(self.child.stdout, selectors.EVENT_READ)
        try:
            hello = self.read()
            if hello.get('event') != 'hello' or hello.get('protocol_version') != 1:
                raise ValueError('unsupported session handshake')
        except Exception:
            self.close()
            raise

    def read(self):
        while b'\n' not in self.buffer:
            remaining = self.deadline - time.monotonic()
            if remaining <= 0 or not self.selector.select(remaining):
                raise TimeoutError('CartPole session deadline')
            data = os.read(self.child.stdout.fileno(), 65536)
            if not data:
                raise RuntimeError('session ended; inspect stderr.log')
            self.buffer += data
            if len(self.buffer) > 1024*1024:
                raise ValueError('oversized session response')
        line, self.buffer = self.buffer.split(b'\n', 1)
        return json.loads(line)

    def call(self, command, expected='ok'):
        if time.monotonic() >= self.deadline:
            raise TimeoutError('CartPole session deadline')
        self.child.stdin.write((json.dumps(command) + '\n').encode())
        self.child.stdin.flush()
        event = self.read()
        observed = event.get('time')
        if event.get('event') != expected:
            raise ValueError('unexpected session response: ' + str(event))
        if type(observed) not in (float, int) or not math.isfinite(observed):
            raise ValueError('invalid response time')
        return event

    def state(self):
        event = self.call({'command': 'state'}, 'state')
        if abs(event['time'] - self.time) > 1e-9:
            raise ValueError('stale observation')
        values = [event['values'][name] for name in ['x', 'v', 'theta', 'omega']]
        if not all(type(v) in (int, float) and math.isfinite(v) for v in values):
            raise ValueError('invalid physical state')
        return values

    def set_force(self, force):
        if not math.isfinite(force) or abs(force) > 10:
            raise ValueError('force outside policy bounds')
        event = self.call({'command': 'set_inputs', 'inputs': [['force', force]]})
        if abs(event['time'] - self.time) > 1e-9:
            raise ValueError('input update advanced time')

    def reset(self):
        self.set_force(0.0)
        event = self.call({'command': 'reset'})
        if event['time'] != 0:
            raise ValueError('reset did not return time zero')
        self.time = 0.0
        return self.state()

    def step(self, force):
        self.set_force(force)
        target = self.time + .02
        event = self.call({'command': 'advance_to', 'time': target})
        if abs(event['time'] - target) > 1e-9:
            raise ValueError('step time mismatch')
        self.time = event['time']
        return self.state()

    def close(self):
        try:
            if self.child.poll() is None:
                try:
                    self.child.stdin.write(b'{"command":"close"}\n')
                    self.child.stdin.flush()
                    self.child.wait(timeout=3)
                except (BrokenPipeError, subprocess.TimeoutExpired):
                    pass
        finally:
            if self.child.poll() is None:
                self.child.kill()
                self.child.wait(timeout=3)
            self.selector.close()
            self.log.close()
        return self.child.returncode


def force_for(weights, state):
    scales = [2.4, 2.0, math.radians(12), 2.0]
    return 10 * math.tanh(sum(w*x/s for w, x, s in zip(weights, state, scales)))


def episode(session, weights, trace=None):
    state = session.reset()
    for step in range(1, 501):
        force = force_for(weights, state)
        state = session.step(force)
        failed = abs(state[0]) > 2.4 or abs(state[2]) > math.radians(12)
        if trace is not None:
            trace.writerow([session.time, *state, force, failed])
        if failed:
            return step
    return 500


def search(session, rng, generations, population):
    mean, deviation = [0.0]*4, [1.0]*4
    best, best_score, history = mean[:], 0, []
    for generation in range(generations):
        candidates = [[rng.gauss(m, s) for m, s in zip(mean, deviation)]
                      for _ in range(population)]
        scored = [(episode(session, weights), weights) for weights in candidates]
        scored.sort(key=lambda item: item[0], reverse=True)
        if scored[0][0] > best_score:
            best_score, best = scored[0]
        elites = [weights for _, weights in scored[:max(2, population//4)]]
        mean = [sum(w[j] for w in elites)/len(elites) for j in range(4)]
        deviation = [max(.1, math.sqrt(sum((w[j]-mean[j])**2 for w in elites)/len(elites)))
                     for j in range(4)]
        row = {'generation': generation+1, 'best_steps': best_score,
               'population_mean_steps': sum(score for score, _ in scored)/population}
        history.append(row)
        print(json.dumps(row), flush=True)
    return best, history


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--rumoca', default='rumoca')
    parser.add_argument('--output', type=Path, default=Path('cartpole-run'))
    parser.add_argument('--generations', type=int, default=8)
    parser.add_argument('--population', type=int, default=16)
    parser.add_argument('--seed', type=int, default=1)
    parser.add_argument('--wall-seconds', type=int, default=180)
    args = parser.parse_args()
    if args.population < 4 or min(args.generations, args.wall_seconds) < 1:
        parser.error('positive generations/wall budget and population >=4 required')
    args.output.mkdir(exist_ok=False)
    result = {'completed': False, 'seed': args.seed, 'method': 'cross-entropy policy search',
              'scope': 'fixed-start instructional CartPole, not Gymnasium benchmark acceptance'}
    started = time.monotonic()
    with tempfile.TemporaryDirectory(prefix='cartpole-') as scratch_name:
        scratch, session = Path(scratch_name), None
        try:
            session = Session(args.rumoca, scratch, started + args.wall_seconds)
            result['zero_policy_steps'] = episode(session, [0.0]*4)
            weights, history = search(session, random.Random(args.seed), args.generations, args.population)
            with (args.output/'flight.csv').open('w', newline='') as handle:
                writer = csv.writer(handle)
                writer.writerow(['time', 'x', 'v', 'theta', 'omega', 'force', 'failed'])
                result['frozen_steps'] = episode(session, weights, writer)
            result.update(completed=True, weights=weights, training=history)
        except Exception as error:
            result['error'] = repr(error)
            raise
        finally:
            if session is not None:
                result['child_exit_code'] = session.close()
            if (scratch/'stderr.log').exists():
                shutil.copy2(scratch/'stderr.log', args.output/'stderr.log')
            result['elapsed_seconds'] = time.monotonic() - started
            (args.output/'result.json').write_text(json.dumps(result, indent=2)+'\n')
            print(json.dumps(result), flush=True)


if __name__ == '__main__':
    main()
