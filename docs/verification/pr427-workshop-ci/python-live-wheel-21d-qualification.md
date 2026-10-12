# Installed-wheel live export qualification

Compiler source `21d64ecaa9d205b671598991ee8c4d2f556fed6c`; test source `4fad761ae766e7d4b2dbe5965f88ba164e0de0ea`. The actual installed native Linux x86_64 wheel passed all five script checks with required JAX 0.9.2, CasADi 3.7.2 and NumPy 2.4.3. Imports resolved to the owned virtual environment’s installed `site-packages/rumoca`, not a source checkout.

The original `UsesLib` public Session/model paths check RHS values and state/parameter derivatives at two independent points: JAX JIT, forward and reverse AD; CasADi symbolic Jacobians. Additional controls exercise the notebook magic’s live object, dependent parameter/state defaults (`a=2`, `b=6`, `x=6`, RHS `-36`), and missing-dependency ImportErrors even with both packages installed. The dependent-default hypothesis did not reproduce a defect; no Python initialization repair is justified by this result.

The existing wheel smoke CI owner now requires both backends on Linux x86_64 GNU and installs the exact pins. Other platforms retain their documented optional dependencies. This local proof uses a dev-profile native Linux wheel; release/zig/manylinux packaging and fresh remote CI are still unrun.

Build through `nix develop .#full` with the owned Cargo/TMPDIR cache:

```sh
unset PYTHONPATH
export PYO3_PYTHON="$HOME/scratch/rumoca/workshop427-exports/cache/venv/bin/python"
maturin build --compatibility linux --out "$HOME/scratch/rumoca/workshop427-exports/wheels" --manifest-path crates/rumoca-bind-python/Cargo.toml
```

Install that actual wheel into the owned virtual environment with `pip install --force-reinstall --no-deps`, then execute:

```sh
python crates/rumoca-bind-python/tests/export_test.py --require-backend jax --require-backend casadi
```

The adjacent JSON pins wheel/native-library/raw-log/guardian/source-manifest hashes. Build elapsed 291.69 s, peak group RSS 2,903,028 KiB; live script elapsed 4.91 s, peak 358,508 KiB. Both guardians exited 0 without a resource stop. At this original compiler source the neural-model JAX/CasADi initialization refusal remained a workshop blocker; this small wheel proof does not claim all authored examples work. The later [static Integer export qualification](static-integer-export-qualification.md) records the actual shared-owner repair and ten passing backend cells at source `92f916958`.
