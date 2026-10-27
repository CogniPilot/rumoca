"""Small NumPy PPO core for four continuous, tanh-mapped control channels.

The stored Gaussian latent action defines the score-function likelihood.
The deterministic tanh/physical-unit mapping is part of the environment action;
ratios use the same latent action under old/new policies, never discrete bins.
"""
import math
import numpy as np


class MLP:
    def __init__(self, inputs, outputs, rng, hidden=(64, 64), output_scale=1.0):
        widths = (inputs, *hidden, outputs)
        self.params = {}
        for layer, (left, right) in enumerate(zip(widths, widths[1:])):
            scale = output_scale if layer == len(widths) - 2 else 1.0
            self.params[f'W{layer}'] = rng.normal(size=(left, right)) * scale / math.sqrt(left)
            self.params[f'b{layer}'] = np.zeros(right, dtype=np.float64)

    def forward(self, x, cache=False):
        x = np.asarray(x, dtype=np.float64)
        if x.ndim != 2 or not np.isfinite(x).all():
            raise ValueError('network inputs must be a finite matrix')
        activations = [x]
        layers = len(self.params) // 2
        for layer in range(layers):
            x = x @ self.params[f'W{layer}'] + self.params[f'b{layer}']
            if layer != layers - 1:
                x = np.tanh(x)
            activations.append(x)
        if not np.isfinite(x).all():
            raise FloatingPointError('nonfinite network output')
        return (x, activations) if cache else x

    def backward(self, activations, output_gradient):
        gradient = np.asarray(output_gradient, dtype=np.float64)
        grads = {}
        for layer in reversed(range(len(self.params) // 2)):
            grads[f'W{layer}'] = activations[layer].T @ gradient
            grads[f'b{layer}'] = gradient.sum(axis=0)
            if layer:
                gradient = (gradient @ self.params[f'W{layer}'].T) * (1 - activations[layer] ** 2)
        return grads


def gaussian_log_prob(latent, mean, log_std):
    normalized = (latent - mean) * np.exp(-log_std)
    return -0.5 * np.sum(normalized ** 2 + 2 * log_std + math.log(2 * math.pi), axis=1)


def actor_loss_gradient(actor, log_std, observations, latent, old_log_prob, advantage,
                        clip=0.2, entropy_coefficient=0.001):
    mean, cache = actor.forward(observations, cache=True)
    log_prob = gaussian_log_prob(latent, mean, log_std)
    log_ratio = log_prob - old_log_prob
    bounded_log_ratio = np.clip(log_ratio, -20, 20)
    ratio = np.exp(bounded_log_ratio)
    clipped_ratio = np.clip(ratio, 1 - clip, 1 + clip)
    entropy = np.sum(log_std + 0.5 * (1 + math.log(2 * math.pi)))
    loss = -np.minimum(ratio * advantage, clipped_ratio * advantage).mean() - entropy_coefficient * entropy
    inactive = ((advantage > 0) & (ratio > 1 + clip)) | ((advantage < 0) & (ratio < 1 - clip))
    inactive |= (log_ratio < -20) | (log_ratio > 20)
    d_log_prob = np.where(inactive, 0.0, -advantage * ratio) / len(observations)
    residual = latent - mean
    inv_variance = np.exp(-2 * log_std)
    grads = actor.backward(cache, d_log_prob[:, None] * residual * inv_variance)
    grads['log_std'] = np.sum(d_log_prob[:, None] * (residual ** 2 * inv_variance - 1), axis=0)
    grads['log_std'] -= entropy_coefficient
    metrics = {'actor_loss': float(loss), 'entropy': float(entropy),
               'approx_kl': float(np.mean((ratio - 1) - bounded_log_ratio)),
               'clip_fraction': float(np.mean(np.abs(ratio - 1) > clip))}
    if not math.isfinite(float(loss)) or not all(np.isfinite(g).all() for g in grads.values()):
        raise FloatingPointError('nonfinite PPO actor loss/gradient')
    return float(loss), grads, metrics


def critic_loss_gradient(critic, observations, returns):
    prediction, cache = critic.forward(observations, cache=True)
    error = prediction[:, 0] - returns
    loss = 0.5 * np.mean(error ** 2)
    grads = critic.backward(cache, (error / len(observations))[:, None])
    if not math.isfinite(float(loss)) or not all(np.isfinite(g).all() for g in grads.values()):
        raise FloatingPointError('nonfinite critic loss/gradient')
    return float(loss), grads


def generalized_advantages(reward, value, next_value, terminated, truncated,
                           gamma=0.99, lam=0.95):
    """Bootstrap time limits from the OLD terminal observation; stop GAE at reset."""
    result = np.empty_like(reward, dtype=np.float64)
    running = 0.0
    for index in reversed(range(len(reward))):
        delta = reward[index] + gamma * (not terminated[index]) * next_value[index] - value[index]
        continuation = not (terminated[index] or truncated[index])
        running = delta + gamma * lam * continuation * running
        result[index] = running
    return result, result + value


class Adam:
    def __init__(self, params, learning_rate=3e-4, max_gradient_norm=0.5):
        self.params = params
        self.first = {name: np.zeros_like(value) for name, value in params.items()}
        self.second = {name: np.zeros_like(value) for name, value in params.items()}
        self.steps = 0
        self.learning_rate = learning_rate
        self.max_gradient_norm = max_gradient_norm

    def step(self, grads):
        if grads.keys() != self.params.keys():
            raise ValueError('gradient/parameter roster mismatch')
        norm = math.sqrt(sum(float(np.sum(g * g)) for g in grads.values()))
        if not math.isfinite(norm):
            raise FloatingPointError('nonfinite gradient norm')
        factor = min(1.0, self.max_gradient_norm / max(norm, 1e-12))
        self.steps += 1
        for name, parameter in self.params.items():
            gradient = grads[name] * factor
            self.first[name] *= 0.9
            self.first[name] += 0.1 * gradient
            self.second[name] *= 0.999
            self.second[name] += 0.001 * gradient ** 2
            first = self.first[name] / (1 - 0.9 ** self.steps)
            second = self.second[name] / (1 - 0.999 ** self.steps)
            parameter -= self.learning_rate * first / (np.sqrt(second) + 1e-8)
            if not np.isfinite(parameter).all():
                raise FloatingPointError('nonfinite parameter update')
        return norm


def physical_action(normalized):
    action = np.asarray(normalized, dtype=np.float64)
    if action.shape != (4,) or not np.isfinite(action).all() or np.any(np.abs(action) > 1):
        raise ValueError('action must have four finite normalized channels')
    return np.array([(action[0] + 1) * 4, action[1] * 4, action[2] * 2.5, action[3]], dtype=np.float64)
