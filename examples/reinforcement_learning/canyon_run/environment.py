"""Existing Rumoca stdio session as a continuous-control training environment."""
import hashlib
import math
import os
from pathlib import Path
import shutil
import tempfile
import numpy as np
from ppo import physical_action

import stdio as transport

EXAMPLE = Path(__file__).resolve().parent
REPO = EXAMPLE.parents[2]
MODEL = REPO / 'examples/interactive/canyon_run/CanyonRun.mo'
BINARY = None
CMM = None


def configure(binary, source_root):
    global BINARY, CMM
    located = shutil.which(str(binary))
    if located is None:
        raise FileNotFoundError('rumoca executable not found: ' + str(binary))
    BINARY = Path(located).resolve()
    CMM = Path(source_root).resolve()
    if not CMM.is_dir() or not (CMM / 'RigidBody').is_dir():
        raise FileNotFoundError('CMM source root missing; run cargo xtask repo modelica-deps ensure, or pass --source-root')

INPUTS = ['thrustAcceleration', 'pRateDemand', 'qRateDemand', 'rRateDemand']


def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def numeric(state, name):
    value = state[name]
    if type(value) not in (int, float) or not math.isfinite(value):
        raise ValueError('nonfinite/non-numeric state: ' + name)
    return float(value)


def flag(state, name):
    # The actual version1 state event is a real-valued named-variable map,
    # including Boolean outputs (the typed get event is a different schema).
    value = numeric(state, name)
    if value not in (0, 1):
        raise ValueError('state Boolean is not exactly0/1: ' + name)
    return value == 1


class ContinuousEnv:
    def __init__(self, output, wall_seconds=1800, cpu=14):
        self.output = Path(output)
        self.scratch = Path(tempfile.mkdtemp(prefix='rumoca-canyon-training-'))
        self.session = None
        self.dt = .05
        self.steps = 0
        self.previous = np.array([-.5, 0, 0, 0], dtype=np.float64)
        try:
            if BINARY is None or CMM is None:
                raise ValueError('configure the executable/source root before starting the environment')
            self.model = self.output / 'CanyonRun.mo'
            shutil.copy2(MODEL, self.model)
            (self.output / 'INPUTS.json').write_text(__import__('json').dumps({
                'model_sha256': sha(self.model), 'binary_sha256': sha(BINARY),
                'cmm_files_sha256': {str(p.relative_to(CMM)): sha(p) for p in sorted(CMM.rglob('*.mo'))}}, indent=2) + '\n')
            argv = ['prlimit', '--as=4294967296', str(BINARY), 'sim', str(self.model),
                    '-m', 'CanyonRun.ContinuousControl.Run', '--source-root', str(CMM),
                    '--solver', 'rk-like', '--serve-stdio', '--t-end', '1',
                    '--cache-dir', str(self.scratch / 'cache')]
            self.session = transport.Session(argv, os.environ.copy(), self.output, cpu, wall_seconds, 2 * 1024**3)
            self.hello = self.session.read()
            if self.hello.get('event') != 'hello' or type(self.hello.get('protocol_version')) is not int or self.hello['protocol_version'] != 1:
                raise ValueError('wrong native session handshake')
        except Exception:
            self.close()
            raise

    def _set(self, controls):
        transport.finite(self.session.time)
        event = self.session.call({'command': 'set_inputs', 'inputs': list(zip(INPUTS, map(float, controls)))}, 'ok')
        if type(event.get('time')) not in (int, float) or not math.isfinite(event['time']) or abs(event['time'] - self.session.time) > 1e-8:
            raise ValueError('input update changed session time')

    def _read(self):
        state = self.session.state()
        obs = np.array([numeric(state, f'observation[{i}]') for i in range(1, 87)])
        self.current_position = np.array([numeric(state, f'position[{i}]') for i in range(1, 4)])
        self.current_quat = np.array([numeric(state, f'quat[{i}]') for i in range(1, 5)])
        if abs(np.linalg.norm(self.current_quat) - 1) > 1e-3:
            raise ValueError('invalid native quaternion norm')
        self.current_speed = numeric(state, 'airspeed')
        self.current_actuators = np.array([numeric(state, f'actuators[{i}]') for i in range(1, 5)])
        self.current_control = np.array([numeric(state, f'appliedControl[{i}]') for i in range(1, 5)])
        self.terminated = flag(state, 'done')
        self.finished = flag(state, 'finished')
        if self.terminated != (flag(state, 'crashed') or self.finished):
            raise ValueError('native termination flags disagree')
        return obs

    def reset(self):
        # Set the next initial control BEFORE reset, so reset initialization and
        # the time-zero previous-action observation describe the same trim input.
        self._set([2, 0, 0, 0])
        event = self.session.call({'command': 'reset'}, 'ok')
        if type(event.get('time')) not in (int, float) or event['time'] != 0:
            raise ValueError('native reset did not return to time0')
        self.session.time = 0
        self.steps = 0
        self.previous = np.array([-.5, 0, 0, 0], dtype=np.float64)
        obs = self._read()
        if not np.allclose(self.current_control, [2, 0, 0, 0], atol=1e-12, rtol=0):
            raise ValueError('reset did not bind trim control')
        return obs

    def step(self, normalized):
        checked_time = transport.finite(self.session.time)
        controls = physical_action(normalized)
        before = self.current_position.copy()
        self._set(controls)
        target = transport.finite(checked_time + self.dt)
        event = self.session.call({'command': 'advance_to', 'time': target}, 'ok')
        if type(event.get('time')) not in (int, float) or not math.isfinite(event['time']) or abs(event['time'] - target) > 1e-8:
            raise ValueError('native step time mismatch')
        self.session.time = event['time']
        obs = self._read()
        if not np.allclose(self.current_control, controls, atol=1e-10, rtol=0):
            raise ValueError('requested/applied simultaneous control mismatch')
        self.steps += 1
        x, y, z = self.current_position
        center = 10 * math.sin(.0349 * x) + 1.5 * math.sin(.0898 * x + 1.3)
        width = 16 + 3 * math.sin(.0524 * x + .7)
        components = {'progress': .1 * (x - before[0]),
                      'centering': -.02 * ((y - center) / width) ** 2,
                      'height': -.10 * ((z - 12) / 12) ** 2,
                      'rates': -.001 * float(np.sum(normalized[1:] ** 2)),
                      'smoothness': -.01 * float(np.sum((normalized - self.previous) ** 2)),
                      'collision': -5.0 if self.terminated and not self.finished else 0.0,
                      'finish': 5.0 if self.finished else 0.0}
        self.previous = np.asarray(normalized).copy()
        truncated = not self.terminated and self.steps >= 1200
        info = {'distance': float(x), 'position': self.current_position.tolist(),
                'airspeed': self.current_speed, 'quaternion': self.current_quat.tolist(), 'observation': obs.tolist(), 'actuators': self.current_actuators.tolist(), 'applied': self.current_control.tolist(),
                'components': components, 'finished': self.finished, 'sim_time': self.session.time}
        return obs, float(sum(components.values())), self.terminated, truncated, info

    def close(self):
        result = None
        if self.session is not None:
            result = {'peak_rss': self.session.peak, 'resource_failure': self.session.failure}
            if self.session.child.poll() is None and self.session.failure is None:
                try:
                    self.session.call({'command': 'close'}, 'closed')
                    self.session.child.wait(timeout=5)
                except Exception as error:
                    result['graceful_close_error'] = repr(error)
            result['cleanup'] = self.session.finish()
            self.session = None
        if self.scratch.exists():
            shutil.rmtree(self.scratch)
        return result
