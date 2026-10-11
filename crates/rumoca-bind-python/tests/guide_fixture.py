"""Stage unchanged repository examples in a guide test's owned workspace."""

from pathlib import Path
import shutil


REPOSITORY = Path(__file__).resolve().parents[3]
EXAMPLES = (
    "models/SympyDecay.mo",
    "models/GalecCounter.mo",
    "simulation/rumoca-scenario.sympy_decay.toml",
)


def stage_examples(workspace):
    for relative in EXAMPLES:
        source = REPOSITORY / "examples" / relative
        destination = workspace / "examples" / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copyfile(source, destination)
        assert destination.read_bytes() == source.read_bytes()
