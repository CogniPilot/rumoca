#!/usr/bin/env python3
import sys, os, subprocess, signal, select

log = open("/tmp/bridge_wrapper_log.txt","w",buffering=1)
env = os.environ.copy()
env["CUBS2_ZEPHYR_BRIDGE_DEBUG"] = "1"

bridge_script = os.path.join(os.path.dirname(__file__), "fixed_wing_zephyr_bridge.py")
proc = subprocess.Popen([sys.executable, bridge_script], env=env,
    stdout=subprocess.PIPE, stderr=subprocess.STDOUT)

def stop(s, f):
    if proc.poll() is None: proc.terminate()
    raise KeyboardInterrupt
signal.signal(signal.SIGTERM, stop)
signal.signal(signal.SIGINT, stop)

while proc.poll() is None:
    r,_,_ = select.select([proc.stdout], [], [], 0.2)
    if r:
        line = proc.stdout.readline()
        if line: log.write(line.decode(errors='replace'))
    else:
        log.write("(waiting)\n")
    log.flush()
for line in proc.stdout.readlines():
    log.write(line.decode(errors='replace'))
log.write(f"exit={proc.returncode}\n")
log.close()
