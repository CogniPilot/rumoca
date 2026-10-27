"""Nominal frozen-policy development flight; no training or held-out claim."""
import argparse
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import shutil
import time
import numpy as np
from environment import ContinuousEnv, MODEL, REPO, configure, sha
from ppo import MLP


def load_actor(checkpoint):
    actor = MLP(86, 4, np.random.default_rng(0))
    with np.load(checkpoint, allow_pickle=False) as saved:
        for name, destination in actor.params.items():
            value = saved['actor_' + name]
            if value.shape != destination.shape or not np.isfinite(value).all():
                raise ValueError('invalid actor parameter: ' + name)
            destination[:] = value
    return actor


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--checkpoint', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--wall', type=int, default=120)
    parser.add_argument('--cpu', type=int, default=min(os.sched_getaffinity(0)))
    parser.add_argument('--rumoca', default='rumoca')
    parser.add_argument('--source-root', type=Path, default=REPO / 'target/cmm/CMM-a642c381')
    args = parser.parse_args()
    configure(args.rumoca, args.source_root)
    os.sched_setaffinity(0, {args.cpu})
    args.output.mkdir(exist_ok=False)
    for name in ['evaluate_policy.py', 'environment.py', 'stdio.py', 'ppo.py']:
        shutil.copy2(Path(__file__).with_name(name), args.output / name)
    shutil.copy2(args.checkpoint, args.output / 'policy.npz')
    actor = load_actor(args.output / 'policy.npz')
    original = {name: value.copy() for name, value in actor.params.items()}
    result = {'completed': False, 'purpose': 'one nominal frozen-policy development flight',
              'learning': False, 'action_noise': False,
              'checkpoint_source': str(args.checkpoint),
              'checkpoint_sha256': sha(args.checkpoint), 'steps': 0,
              'cpu': args.cpu, 'wall_seconds': args.wall}
    (args.output / 'START.json').write_text(json.dumps(result, indent=2) + '\n')
    env = None
    started = time.monotonic()
    total_reward = 0.0
    try:
        env = ContinuousEnv(args.output, args.wall, args.cpu)
        obs = env.reset()
        with (args.output / 'flight.jsonl').open('w') as trace:
            for step in range(1, 1201):
                mean = actor.forward(np.clip(obs, -5, 5)[None])[0]
                action = np.tanh(mean)
                obs, reward, terminated, truncated, info = env.step(action)
                total_reward += reward
                trace.write(json.dumps({'step': step, 'raw_mean': mean.tolist(),
                    'normalized_action': action.tolist(), 'reward': reward,
                    'terminated': terminated, 'truncated': truncated, **info}, allow_nan=False) + '\n')
                result.update(steps=step, distance=info['distance'],
                              finished=info['finished'], terminated=terminated,
                              truncated=truncated, return_value=total_reward,
                              terminal_position=info['position'], sim_time=info['sim_time'])
                if terminated or truncated:
                    break
        unchanged = all(np.array_equal(value, original[name]) for name, value in actor.params.items())
        if not unchanged:
            raise ValueError('frozen actor changed during evaluation')
        result.update(completed=True, actor_unchanged=unchanged)
    except Exception as error:
        result['error'] = repr(error)
        raise
    finally:
        if env is not None:
            result['native_close'] = env.close()
        result['elapsed_wall'] = time.monotonic() - started
        result['ended_utc'] = datetime.now(timezone.utc).isoformat()
        (args.output / 'RESULT.json').write_text(json.dumps(result, indent=2) + '\n')
        files = sorted(p for p in args.output.iterdir() if p.is_file())
        (args.output / 'SHA256SUMS').write_text(''.join(sha(p) + '  ' + p.name + '\n' for p in files))
        print(json.dumps(result), flush=True)


if __name__ == '__main__':
    main()
