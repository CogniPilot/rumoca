# CUBS2 flight SIL model

This directory is a notebook-local snapshot of the model composition used by
the `cerebri_cubs2` flight SIL test at upstream commit
`7f2dae4820e9ae8862a596d0f417465cbf725878`.

Upstream: https://github.com/CogniPilot/cerebri_cubs2/tree/7f2dae4820e9ae8862a596d0f417465cbf725878

The complete mission is `Cubs2PatternMission`. It is composed from:

- `SportCubSIL.mo`: the `SportCubPlant` physical/aerodynamic model.
- `FixedWingSIL.mo`: the `FixedWingFBW` inner attitude/rate controller. Its
  legacy `FixedWingPlant` and `FixedWing` classes are not instantiated by the
  CUBS2 mission.
- `FixedWingOuterLoop.mo`: the deployed route guidance, attitude command, and
  TECS controller used to generate CUBS2 eFMI production code.
- `Cubs2FlightScenarios.mo`: the SIL wiring and checked-in pattern route; its
  `Cubs2PatternMission` class is the simulation entry point.

`rumoca-scenario.pattern.toml` preserves the upstream full-mission settings:
70 seconds, 20 ms output interval, and the RK-like solver. The only local
change is the CMM source-root path, which points at Rumoca's pinned dependency
cache.

Run it from `examples/notebooks` with the Python API:

```python
session, model, config = rumoca.Session.from_scenario(
    "cubs2_sil/rumoca-scenario.pattern.toml"
)
result = model.simulate(t=(0.0, 70.0), config=config)
```
