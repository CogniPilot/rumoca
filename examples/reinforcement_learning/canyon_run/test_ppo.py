import unittest
import numpy as np
from ppo import MLP, Adam, actor_loss_gradient, critic_loss_gradient
from ppo import gaussian_log_prob, generalized_advantages, physical_action


class PPOControls(unittest.TestCase):
    def setUp(self):
        self.rng = np.random.default_rng(41)
        self.obs = self.rng.normal(size=(4, 3))

    def finite_differences(self, params, gradients, loss):
        maximum = 0.0
        for name, parameter in params.items():
            for index in np.ndindex(parameter.shape):
                old = parameter[index]
                parameter[index] = old + 1e-6
                high = loss()
                parameter[index] = old - 1e-6
                low = loss()
                parameter[index] = old
                reference = (high - low) / 2e-6
                error = abs(reference - gradients[name][index]) / max(1, abs(reference))
                maximum = max(maximum, error)
        self.assertLess(maximum, 2e-6)

    def test_actor_and_log_std_match_full_numerical_gradient(self):
        actor = MLP(3, 4, self.rng, hidden=(4, 3))
        log_std = np.array([-.7, -.5, -.3, -.8])
        latent = self.rng.normal(size=(4, 4))
        current = gaussian_log_prob(latent, actor.forward(self.obs), log_std)
        old = current - np.log([.7, 1.0, 1.1, 1.3])
        advantage = np.array([-1., 2., -.5, 1.])
        def evaluate():
            return actor_loss_gradient(actor, log_std, self.obs, latent, old, advantage)
        _, gradients, _ = evaluate()
        self.finite_differences({**actor.params, 'log_std': log_std}, gradients,
                                lambda: evaluate()[0])

    def test_clipped_surrogates_stop_both_policy_and_std_gradient(self):
        actor = MLP(3, 4, self.rng, hidden=(4, 3))
        log_std = np.zeros(4)
        obs = self.obs[:2]
        latent = self.rng.normal(size=(2, 4))
        old = gaussian_log_prob(latent, actor.forward(obs), log_std) - np.log([.7, 1.3])
        _, gradients, _ = actor_loss_gradient(actor, log_std, obs, latent, old,
                                              np.array([-1., 1.]), entropy_coefficient=0)
        self.assertTrue(all(np.array_equal(g, np.zeros_like(g)) for g in gradients.values()))

    def test_critic_matches_full_numerical_gradient(self):
        critic = MLP(3, 1, self.rng, hidden=(4, 3))
        returns = np.array([1., -.2, .4, 2.])
        def evaluate():
            return critic_loss_gradient(critic, self.obs, returns)
        _, gradients = evaluate()
        self.finite_differences(critic.params, gradients, lambda: evaluate()[0])

    def test_timeout_bootstraps_old_state_but_does_not_cross_reset(self):
        advantage, returns = generalized_advantages(np.array([1., 4.]), np.array([2., 1.]),
            np.array([3., 99.]), np.array([False, True]), np.array([True, False]), gamma=.9)
        np.testing.assert_allclose(advantage, [1.7, 3.])
        np.testing.assert_allclose(returns, [3.7, 4.])

    def test_nonterminal_rollout_boundary_bootstraps_and_accumulates(self):
        advantage, _ = generalized_advantages(np.array([1., 2.]), np.array([.5, .6]),
            np.array([.6, .8]), np.array([False, False]), np.array([False, False]), gamma=.9, lam=.95)
        np.testing.assert_allclose(advantage, [1.04 + .9*.95*2.12, 2.12])

    def test_adam_decreases_external_quadratic_and_action_mapping_is_continuous(self):
        x = np.array([2., -3.])
        initial_loss = .5 * np.sum(x*x)
        optimizer = Adam({'x': x})
        optimizer.step({'x': x.copy()})
        self.assertLess(.5 * np.sum(x*x), initial_loss)
        np.testing.assert_allclose(physical_action([-.5, .1, -.2, .3]), [2, .4, -.5, .3])
        difference = physical_action([-.5, .10001, -.2, .3]) - physical_action([-.5, .1, -.2, .3])
        self.assertAlmostEqual(difference[1], .00004)
        with self.assertRaises(ValueError):
            physical_action([0, 0, 0, np.nan])


if __name__ == '__main__':
    unittest.main(verbosity=2)
