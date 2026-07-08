# TODO

- CUBS2 protected access: `Cubs2AltitudeHold` reads
  `outerLoop.guidance.pathAltitude`, but `pathAltitude` is protected inside
  `FixedWingOuterLoop.RouteGuidance`. Expose a public telemetry value on
  `FixedWingOuterLoop` or otherwise consume a public signal instead of reading
  the protected component.
- Python binding PathLike cleanup: file/path arguments now accept
  `os.PathLike`, but source-root lists are still documented as `Sequence[str]`.
  If roots should accept `PathLike`, implement that through `Session` argument
  parsing so caches remain owned by the session.
