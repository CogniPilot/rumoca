#!/usr/bin/env python3
"""Wrapper for bridge that logs everything to file."""
import os
import sys
os.environ["CUBS2_ZEPHYR_BRIDGE_DEBUG"] = "1"
os.environ["CUBS2_ZEPHYR_EXE"] = "/home/prady/code/purt/cognipilot/ws/cerebri/cubs2/build/zephyr/zephyr.exe"

sys.path.insert(0, "/home/prady/code/purt/rumoca/examples/interactive/fixed_wing_binary")

log = open("/tmp/bridge_debug.log", "w", buffering=1)
log.write(f"Starting bridge at {__import__('datetime').datetime.now()}\n")
log.write(f"CUBS2_ZEPHYR_EXE={os.environ.get('CUBS2_ZEPHYR_EXE')}\n")
log.write(f"CUBS2_ZEPHYR_BRIDGE_DEBUG={os.environ.get('CUBS2_ZEPHYR_BRIDGE_DEBUG')}\n")
log.flush()

# Monkey-patch print to also go to log
orig_print = __builtins__.print
def debug_print(*args, **kwargs):
    orig_print(*args, **kwargs)
    log.write(" ".join(str(a) for a in args) + "\n")
    log.flush()
__builtins__.print = debug_print

from fixed_wing_zephyr_bridge import main
try:
    sys.exit(main())
except Exception as e:
    log.write(f"EXCEPTION: {e}\n")
    import traceback
    traceback.print_exc(file=log)
    log.flush()
    raise
