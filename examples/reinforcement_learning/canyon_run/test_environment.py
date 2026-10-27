"""Wire-metadata rejection controls without launching a simulator."""
import math
import unittest
from environment import ContinuousEnv, numeric


class InertSession:
    time = 0.0

    def __init__(self, response_time):
        self.response_time = response_time

    def call(self, command, expected):
        return {'event': expected, 'time': self.response_time}


class MetadataTests(unittest.TestCase):
    def test_input_ack_requires_a_finite_numeric_current_time(self):
        for invalid in [False, True, None, '0', math.nan, math.inf, .01]:
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                env = ContinuousEnv.__new__(ContinuousEnv)
                env.session = InertSession(invalid)
                env._set([2, 0, 0, 0])
        env = ContinuousEnv.__new__(ContinuousEnv)
        env.session = InertSession(0.0)
        env._set([2, 0, 0, 0])

    def test_state_values_reject_bool_string_and_nonfinite(self):
        for invalid in [False, True, '1', None, math.nan, math.inf]:
            with self.subTest(invalid=invalid), self.assertRaises(ValueError):
                numeric({'x': invalid}, 'x')
        self.assertEqual(numeric({'x': 1.25}, 'x'), 1.25)


if __name__ == '__main__':
    unittest.main()
