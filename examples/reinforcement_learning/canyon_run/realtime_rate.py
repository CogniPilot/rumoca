"""Rolling simulated-seconds / wall-seconds display across episode resets."""
from collections import deque
import math


class RealtimeRate:
    def __init__(self, window_seconds=2.0):
        self.window = window_seconds
        self.points = deque()
        self.simulated = 0.0

    def sample(self, delta_simulated, started, ended):
        if not all(math.isfinite(v) for v in (delta_simulated, started, ended)):
            raise ValueError('rate samples must be finite')
        if delta_simulated <= 0 or ended <= started:
            raise ValueError('rate sample must advance simulation and wall time')
        if not self.points:
            self.points.append((started, self.simulated))
        elif started < self.points[-1][0]:
            raise ValueError('wall clock moved backwards')
        self.simulated += delta_simulated
        self.points.append((ended, self.simulated))
        while len(self.points) > 2 and self.points[1][0] < ended - self.window:
            self.points.popleft()
        before, simulated_before = self.points[0]
        return (self.simulated - simulated_before) / (ended - before)
