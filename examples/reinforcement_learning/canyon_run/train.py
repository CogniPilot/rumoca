"""Development PPO training over the released Rumoca CLI, no compiler build."""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import math
import os
from pathlib import Path
import shutil
import statistics
import time
import numpy as np
from environment import ContinuousEnv, MODEL, REPO, configure, sha
from ppo import MLP, Adam, gaussian_log_prob, actor_loss_gradient
from ppo import critic_loss_gradient, generalized_advantages


def preprocess(obs):
    return np.clip(obs, -5, 5)


def episode_summary(rows):
    selected = rows[-50:]
    return {'episodes': len(rows), 'finishes': sum(r['cause'] == 'finish' for r in rows),
            'last50_mean_distance': statistics.mean(r['distance'] for r in selected) if selected else None,
            'last50_mean_return': statistics.mean(r['return'] for r in selected) if selected else None,
            'max_distance': max((r['distance'] for r in rows), default=None),
            'causes': {c: sum(r['cause'] == c for r in rows) for c in ['wall', 'floor', 'rim', 'slow', 'finish', 'timeout']}}


def collect(env, actor, critic, log_std, rng, count, carry, journal, total):
    data = {name: [] for name in ['obs', 'latent', 'logp', 'value', 'reward', 'next_value',
                                  'terminated', 'truncated', 'next_obs', 'applied', 'position']}
    completed = []
    obs, episode_return, episode_length = carry
    for offset in range(count):
        encoded = preprocess(obs)
        mean = actor.forward(encoded[None])[0]
        latent = mean + np.exp(log_std) * rng.normal(size=4)
        normalized = np.tanh(latent)
        logp = gaussian_log_prob(latent[None], mean[None], log_std)[0]
        value = critic.forward(encoded[None])[0, 0]
        next_obs, reward, terminated, truncated, info = env.step(normalized)
        next_value = 0.0 if terminated else critic.forward(preprocess(next_obs)[None])[0, 0]
        values = {'obs': encoded, 'latent': latent, 'logp': logp, 'value': value,
                  'reward': reward, 'next_value': next_value, 'terminated': terminated,
                  'truncated': truncated, 'next_obs': next_obs, 'applied': info['applied'],
                  'position': info['position']}
        for name in data:
            data[name].append(values[name])
        episode_return += reward
        episode_length += 1
        if terminated or truncated:
            _, _, z = info['position']
            cause = ('finish' if info['finished'] else 'timeout' if truncated else
                     'floor' if z <= 0 else 'rim' if z > 30 else 'slow' if info['airspeed'] < 4 else 'wall')
            record = {'global_step': total + offset + 1, 'return': float(episode_return),
                      'length': episode_length, 'distance': info['distance'], 'cause': cause,
                      'terminal_position': info['position'], 'sim_time': info['sim_time']}
            journal.write(json.dumps(record, allow_nan=False) + '\n')
            journal.flush()
            completed.append(record)
            # next_value above was computed BEFORE resetting the native world.
            obs = env.reset()
            episode_return, episode_length = 0.0, 0
        else:
            obs = next_obs
    return {k: np.asarray(v) for k, v in data.items()}, (obs, episode_return, episode_length), completed


def optimize(data, actor, critic, log_std, actor_optimizer, critic_optimizer, rng, epochs, batch_size):
    advantage, returns = generalized_advantages(data['reward'], data['value'], data['next_value'],
                                               data['terminated'], data['truncated'])
    advantage = (advantage - advantage.mean()) / max(float(advantage.std()), 1e-8)
    metrics = {}
    updates = 0
    for epoch in range(epochs):
        order = rng.permutation(len(advantage))
        for start in range(0, len(order), batch_size):
            indices = order[start:start + batch_size]
            _, grads, metrics = actor_loss_gradient(actor, log_std, data['obs'][indices],
                data['latent'][indices], data['logp'][indices], advantage[indices])
            if metrics['approx_kl'] > .03:
                return {**metrics, 'optimizer_steps': updates, 'early_stop_kl': True}
            actor_optimizer.step(grads)
            np.clip(log_std, -3, -.3, out=log_std)
            value_loss, value_grads = critic_loss_gradient(critic, data['obs'][indices], returns[indices])
            critic_optimizer.step(value_grads)
            metrics['critic_loss'] = value_loss
            updates += 1
    return {**metrics, 'optimizer_steps': updates, 'early_stop_kl': False}


def checkpoint(path, actor, critic, log_std, actor_optimizer, critic_optimizer, rng, step):
    arrays = {'log_std': log_std.copy()}
    for prefix, network, optimizer in [('actor', actor, actor_optimizer), ('critic', critic, critic_optimizer)]:
        arrays.update({prefix + '_' + name: value for name, value in network.params.items()})
        arrays.update({prefix + '_adam_m_' + name: value for name, value in optimizer.first.items()})
        arrays.update({prefix + '_adam_v_' + name: value for name, value in optimizer.second.items()})
        arrays[prefix + '_adam_steps'] = np.array(optimizer.steps)
    np.savez_compressed(path, **arrays)
    path.with_suffix('.json').write_text(json.dumps({'global_step': step, 'rng_state': rng.bit_generator.state,
        'checkpoint_sha256': sha(path), 'native_environment_resume': 'NOT_SUPPORTED; any continuation starts a fresh episode and is a new experiment'}, indent=2) + '\n')


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--steps', type=int, default=32768)
    parser.add_argument('--rollout', type=int, default=1024)
    parser.add_argument('--epochs', type=int, default=8)
    parser.add_argument('--batch', type=int, default=128)
    parser.add_argument('--wall', type=int, default=1800)
    parser.add_argument('--cpu', type=int, default=min(os.sched_getaffinity(0)))
    parser.add_argument('--rumoca', default='rumoca')
    parser.add_argument('--source-root', type=Path, default=REPO / 'target/cmm/CMM-a642c381')
    args = parser.parse_args()
    if min(args.steps, args.rollout, args.epochs, args.batch, args.wall) < 1:
        raise ValueError('budgets must be positive')
    configure(args.rumoca, args.source_root)
    os.sched_setaffinity(0, {args.cpu})
    args.output.mkdir(exist_ok=False)
    for name in ['train.py', 'ppo.py', 'environment.py', 'stdio.py', 'test_ppo.py']:
        shutil.copy2(Path(__file__).with_name(name), args.output / name)
    rng = np.random.default_rng(1)
    actor = MLP(86, 4, rng, output_scale=.01)
    critic = MLP(86, 1, rng)
    actor.params['b2'][0] = np.arctanh(-.5)  # Initial mean = 2m/s² forward thrust.
    log_std = np.full(4, math.log(.15))
    actor_optimizer = Adam({**actor.params, 'log_std': log_std})
    critic_optimizer = Adam(critic.params)
    result = {'completed': False, 'method': 'continuous Gaussian latent PPO, tanh physical controls',
              'seed': 1, 'numpy': np.__version__, 'python': os.sys.version,
              'model_sha256': sha(MODEL),
              'steps_requested': args.steps, 'rollout': args.rollout, 'epochs': args.epochs,
              'batch_size': args.batch, 'wall_seconds': args.wall, 'cpu': args.cpu,
              'gamma': .99, 'gae_lambda': .95, 'clip': .2, 'learning_rate': 3e-4,
              'actor_hidden': [64, 64], 'critic_hidden': [64, 64], 'sample_dt': .05,
              'purpose': 'ordinary continuous-controller development; not held-out or paper reproduction'}
    (args.output / 'START.json').write_text(json.dumps(result, indent=2) + '\n')
    env = None
    started = time.monotonic()
    total, rollout_number, episodes = 0, 0, []
    try:
        env = ContinuousEnv(args.output, args.wall, args.cpu)
        (args.output / 'HELLO.json').write_text(json.dumps(env.hello, indent=2) + '\n')
        carry = (env.reset(), 0.0, 0)
        checkpoint(args.output / 'checkpoint-000000.npz', actor, critic, log_std,
                   actor_optimizer, critic_optimizer, rng, 0)
        with (args.output / 'episodes.jsonl').open('w') as journal:
            while total < args.steps:
                count = min(args.rollout, args.steps - total)
                data, carry, completed = collect(env, actor, critic, log_std, rng, count, carry, journal, total)
                total += count
                rollout_number += 1
                episodes.extend(completed)
                np.savez_compressed(args.output / f'rollout-{rollout_number:04}.npz', **data)
                metrics = optimize(data, actor, critic, log_std, actor_optimizer, critic_optimizer,
                                   rng, args.epochs, args.batch)
                checkpoint(args.output / f'checkpoint-{total:06}.npz', actor, critic, log_std,
                           actor_optimizer, critic_optimizer, rng, total)
                progress = {**episode_summary(episodes), 'global_steps': total,
                            'elapsed_wall': time.monotonic() - started, 'log_std': log_std.tolist(), **metrics}
                (args.output / 'PROGRESS.json').write_text(json.dumps(progress, indent=2) + '\n')
                print(json.dumps(progress), flush=True)
        result.update(completed=True, global_steps=total, **episode_summary(episodes))
    except Exception as error:
        result.update(error=repr(error), global_steps=total, **episode_summary(episodes))
        raise
    finally:
        if env is not None:
            result['native_close'] = env.close()
        result['elapsed_wall'] = time.monotonic() - started
        result['ended_utc'] = datetime.now(timezone.utc).isoformat()
        (args.output / 'RESULT.json').write_text(json.dumps(result, indent=2) + '\n')
        files = [p for p in sorted(args.output.iterdir()) if p.is_file()]
        (args.output / 'SHA256SUMS').write_text(''.join(sha(p) + '  ' + p.name + '\n' for p in files))
        print(json.dumps(result), flush=True)


if __name__ == '__main__':
    main()
