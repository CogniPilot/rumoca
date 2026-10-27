import unittest
from realtime_rate import RealtimeRate


class RateTests(unittest.TestCase):
    def test_realtime(self):
        rate = RealtimeRate()
        for step in range(100):
            self.assertAlmostEqual(rate.sample(.05, step*.05, (step+1)*.05), 1)

    def test_fast_rate_and_pause(self):
        rate = RealtimeRate()
        self.assertAlmostEqual(rate.sample(.05, 0, .0025), 20)
        # The pause for a policy update counts as wall time, without sim time.
        self.assertAlmostEqual(rate.sample(.05, .5025, .505), .1/.505)

    def test_episode_reset_does_not_reset_cumulative_simulation_time(self):
        rate = RealtimeRate()
        for step, episode_local_time in enumerate([.05, .1, .15, .05, .1]):
            self.assertAlmostEqual(rate.sample(.05, step*.025, (step+1)*.025), 2)
        self.assertAlmostEqual(rate.simulated, .25)


if __name__ == '__main__':
    unittest.main()
