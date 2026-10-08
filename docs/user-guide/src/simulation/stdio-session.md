# Driving a Session over stdio

`rumoca sim --serve-stdio` runs one simulation session as a child process that
an external program steers: set inputs, advance time, read values, repeat. It is
the entry point for co-simulation masters, hardware-in-the-loop harnesses, and
controllers written in another language. The session uses the real solver (the
same one batch `sim` selects), so events, tolerances, and step control behave as
in a batch run. The mode is headless: there is no viewer and no network
listener.

```bash
rumoca sim --serve-stdio Plant.mo -m Plant --input u=0
```

`--solver`, `--atol`, `--rtol`, `--dt`, and `--t-end` have their batch meaning
(`--t-end` is only the initial horizon; the session extends it on demand).
Initialization reads every input before the controller has written one, so an
input without a default binding needs `--input NAME=VALUE` (repeatable);
otherwise startup fails with `[EX002]`.
`--serve-stdio` cannot be combined with `--config`, `--inspect`, or `--output`.

## Protocol

The controller writes one JSON command per line to the child's stdin and reads
one JSON event per line from its stdout. Each command is answered by exactly one
event, in order. Stdout carries only protocol events; compiler and solver
diagnostics go to stderr.

On startup the child writes a `hello` event with the protocol version:

```json
{"event":"hello","protocol_version":1}
```

### Commands

| Command | Fields | Reply |
|---|---|---|
| `hello` | `protocol_version` | `hello`, or an `EX010` error and exit status 72 when the version is not supported |
| `set_input` | `name`, `value` | `ok` |
| `set_inputs` | `inputs`: `[["name", value], ...]` | `ok` |
| `step` | `dt` (seconds, finite, nonnegative) | `ok` |
| `advance_to` | `time` (absolute seconds, finite) | `ok` |
| `get` | `name` | `value` |
| `state` | none | `state` |
| `reset` | `time` (optional, default `0`) | `ok` |
| `input_names` | none | `input_names` |
| `variable_names` | none | `variable_names` |
| `close` | none | `closed`, then exit status 0 |

An input takes effect on the next advance. `set_inputs` applies a whole frame
atomically and restarts the integrator history once for the frame, so write
coupled signals with one `set_inputs` rather than several `set_input` commands.
Writing a value that is bit-identical to the current input does not restart the
integrator at all.

### Events

| Event | Fields |
|---|---|
| `hello` | `protocol_version` |
| `ok` | `time`: the session time after the command |
| `value` | `name`, `time`, `value` (`null` when `name` is not a variable) |
| `state` | `time`, `values`: every variable by name |
| `input_names`, `variable_names` | `names` |
| `closed` | `time` |
| `error` | `code`, `message` |

JSON has no representation for NaN or infinity; a non-finite value is written as
`null`.

### Errors

A rejected or failed command is answered with an `error` event and the session
stays usable. `code` is a stable diagnostic code:

| Code | Meaning |
|---|---|
| `EX001` | The solver or the session refused the operation (for example an unknown input name, or an integration failure) |
| `EX010` | Unsupported protocol version |
| `EX011` | The line was not a valid command |
| `EX012` | An argument was outside its domain (non-finite or negative time) |

### Exit status

| Status | Meaning |
|---|---|
| `0` | The controller sent `close` |
| `71` | The controller closed stdin or stdout without sending `close` |
| `72` | The controller declared an unsupported protocol version |
| nonzero | The model failed to compile or the session could not be created; the diagnostic is on stderr |

## Example

```text
-> {"command":"set_input","name":"u","value":1.0}
<- {"event":"ok","time":0.0}
-> {"command":"step","dt":0.001}
<- {"event":"ok","time":0.001}
-> {"command":"state"}
<- {"event":"state","time":0.001,"values":{"u":1.0,"y":0.0029,"x":0.0029}}
-> {"command":"close"}
<- {"event":"closed","time":0.001}
```

### Closed loop with an external controller

Given `Plant.mo`:

```modelica
model Plant
  input Real u;
  Real y(start = 0, fixed = true);
equation
  der(y) = -2 * y + 4 * u;
end Plant;
```

A proportional controller
`u = 0.8*(setpoint - y)` that lives in the controlling process closes the loop
through `get`, `set_input`, and `step`:

```python
import json, subprocess

child = subprocess.Popen(
    ["rumoca", "sim", "--serve-stdio", "Plant.mo", "-m", "Plant", "--input", "u=0"],
    stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True,
)
assert json.loads(child.stdout.readline())["event"] == "hello"

def call(**command):
    child.stdin.write(json.dumps(command) + "\n")
    child.stdin.flush()
    return json.loads(child.stdout.readline())

setpoint, dt = 1.0, 0.1
for _ in range(10):
    y = call(command="get", name="y")["value"]
    call(command="set_input", name="u", value=0.8 * (setpoint - y))
    call(command="step", dt=dt)
print(call(command="get", name="y")["value"])  # 0.6143..., approaching 8/13
call(command="close")
```
