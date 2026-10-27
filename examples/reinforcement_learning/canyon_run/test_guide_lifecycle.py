"""Startup refusal and safe spawn controls; no native process is launched."""
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch
import serve
import stdio


class LifecycleTests(unittest.TestCase):
    def test_spawn_uses_taskset_without_python_preexec(self):
        with tempfile.TemporaryDirectory(prefix='rumoca-spawn-test-') as directory:
            with patch.object(stdio.subprocess, 'Popen', side_effect=OSError('inert spawn refusal')) as spawn:
                with self.assertRaises(OSError):
                    stdio.Session(['example-rumoca', 'sim'], {}, Path(directory), 3, 1, 1024)
            self.assertEqual(spawn.call_args.args[0][:4], ['taskset', '--cpu-list', '3', 'example-rumoca'])
            self.assertNotIn('preexec_fn', spawn.call_args.kwargs)
            self.assertTrue(spawn.call_args.kwargs['start_new_session'])

    def check_startup_refusal(self, where):
        original = serve.STATE.copy()
        with tempfile.TemporaryDirectory(prefix='rumoca-startup-test-') as directory:
            serve.CPU, serve.STEPS, serve.WALL = 0, 1, 1
            serve.OUTPUT_ROOT = Path(directory) / 'output'
            serve.STATE.update(running=True, error=None)
            try:
                with patch.object(serve, 'VisibleEnv') as native:
                    if where == 'cpu':
                        with patch.object(serve.os, 'sched_setaffinity', side_effect=OSError('fixture CPU refusal')):
                            serve.train_visible()
                    elif where == 'mkdir':
                        with patch.object(serve.os, 'sched_setaffinity'), patch.object(Path, 'mkdir', side_effect=PermissionError('fixture mkdir refusal')):
                            serve.train_visible()
                    else:
                        with patch.object(serve.os, 'sched_setaffinity'), patch('shutil.copy2', side_effect=OSError('fixture copy refusal')):
                            serve.train_visible()
                    native.assert_not_called()
                self.assertFalse(serve.STATE['running'])
                self.assertIn('fixture', serve.STATE['error'])
            finally:
                serve.STATE.clear()
                serve.STATE.update(original)

    def test_cpu_refusal_releases_the_slot(self):
        self.check_startup_refusal('cpu')

    def test_mkdir_refusal_releases_the_slot(self):
        self.check_startup_refusal('mkdir')

    def test_copy_refusal_releases_the_slot(self):
        self.check_startup_refusal('copy')


if __name__ == '__main__':
    unittest.main()
