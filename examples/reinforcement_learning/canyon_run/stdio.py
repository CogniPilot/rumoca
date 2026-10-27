"""Bounded Linux stdio session for the continuous-control example."""
import json
import math
import os
from pathlib import Path
import selectors
import signal
import subprocess
import threading
import time

def finite(value):
    if type(value) not in (int, float) or not math.isfinite(value):
        raise ValueError("expected finite numeric value")
    return float(value)

class Session:

    def __init__(self, argv, env, directory, cpu, seconds, rss_bytes):
        self.deadline = time.monotonic() + seconds
        self.peak = 0
        self.failure = None
        self.buffer = b''
        self.time = 0.0
        self.stderr = open(directory / 'stderr.log', 'wb')
        self.stop = threading.Event()
        try:
            self.child = subprocess.Popen(['taskset', '--cpu-list', str(cpu), *argv], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=self.stderr, env=env, start_new_session=True)
        except Exception:
            self.stderr.close()
            raise
        try:
            self.selector = selectors.DefaultSelector()
            self.selector.register(self.child.stdout, selectors.EVENT_READ)
        except Exception:
            self._constructor_cleanup()
            raise

        def monitor():
            while not self.stop.wait(0.1):
                rss = 0
                for entry in Path('/proc').iterdir():
                    if not entry.name.isdigit():
                        continue
                    try:
                        fields = (entry / 'stat').read_text().rsplit(')', 1)[1].split()
                        if int(fields[2]) == self.child.pid:
                            rss += int(fields[21]) * os.sysconf('SC_PAGE_SIZE')
                    except (OSError, ValueError, IndexError):
                        pass
                self.peak = max(self.peak, rss)
                if rss > rss_bytes:
                    self.failure = 'RSS_LIMIT'
                elif time.monotonic() >= self.deadline:
                    self.failure = 'WALL_LIMIT'
                elif os.fstat(self.stderr.fileno()).st_size > 1024 * 1024:
                    self.failure = 'STDERR_LIMIT'
                if self.failure:
                    self.kill()
                    return
        self.monitor = threading.Thread(target=monitor, daemon=True)
        try:
            self.monitor.start()
        except Exception:
            self._constructor_cleanup()
            raise

    def _constructor_cleanup(self):
        self.stop.set()
        try:
            os.killpg(self.child.pid, signal.SIGKILL)
        except Exception:
            pass
        try:
            self.child.wait(timeout=5)
        except Exception:
            pass
        monitor = getattr(self, 'monitor', None)
        if monitor is not None and monitor.is_alive():
            monitor.join(timeout=1)
        for name in ('selector',):
            item = getattr(self, name, None)
            if item is not None:
                try:
                    item.close()
                except Exception:
                    pass
        for name in ('stdin', 'stdout'):
            item = getattr(self.child, name, None)
            if item is not None:
                try:
                    item.close()
                except Exception:
                    pass
        try:
            self.stderr.close()
        except Exception:
            pass

    def kill(self):
        try:
            os.killpg(self.child.pid, signal.SIGKILL)
        except ProcessLookupError:
            pass

    def read(self):
        while b'\n' not in self.buffer:
            remaining = self.deadline - time.monotonic()
            if remaining <= 0 or not self.selector.select(max(0, remaining)):
                raise TimeoutError('bounded session response deadline')
            chunk = os.read(self.child.stdout.fileno(), 65536)
            if not chunk:
                raise RuntimeError(self.failure or 'session ended before response')
            self.buffer += chunk
            if len(self.buffer) > 8 * 1024 * 1024:
                raise ValueError('oversized protocol line')
        line, self.buffer = self.buffer.split(b'\n', 1)
        return json.loads(line)

    def call(self, command, expected):
        if time.monotonic() >= self.deadline:
            raise TimeoutError('whole-run deadline')
        self.child.stdin.write((json.dumps(command) + '\n').encode())
        self.child.stdin.flush()
        event = self.read()
        if event.get('event') != expected:
            raise ValueError(f'expected {expected}, got {event}')
        return event

    def state(self):
        event = self.call({'command': 'state'}, 'state')
        if abs(finite(event.get('time')) - finite(self.time)) > 1e-08:
            raise ValueError('state time differs from checked step')
        return event['values']

    def finish(self):
        errors = []
        self.stop.set()
        try:
            self.kill()
        except Exception as error:
            errors.append(error)
        try:
            self.child.wait(timeout=5)
        except Exception as error:
            errors.append(error)
        try:
            self.monitor.join(timeout=1)
        except Exception as error:
            errors.append(error)
        monitor_joined = not self.monitor.is_alive()
        for closeable in (self.selector, self.child.stdin, self.child.stdout, self.stderr):
            try:
                closeable.close()
            except Exception as error:
                errors.append(error)
        clean = self.child.returncode is not None and monitor_joined and (not errors)
        result = {'child_exit_code': self.child.returncode, 'cleanup_confirmed': clean, 'cleanup_errors': [repr(error)[:512] for error in errors]}
        if errors:
            raise RuntimeError('bounded session cleanup incomplete: ' + '; '.join(map(repr, errors)))
        return result
