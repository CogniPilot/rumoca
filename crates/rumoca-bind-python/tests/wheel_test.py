"""Run the shared installed-wheel test roster for CI and cargo xtask python test."""

import argparse
import importlib
from pathlib import Path
import platform
import subprocess
import sys

TESTS = ("smoke_test.py", "fmi_codegen_test.py", "api_test.py", "magic_test.py", "export_test.py")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--require-backend", choices=("jax", "casadi"), action="append", default=[])
    parser.add_argument("--require-guide", action="store_true",
                        help="Require the Linux x86_64 guide, CMake and generated backend checks.")
    parser.add_argument("--installed-root", type=Path,
                        help="Require the imported package to belong to this wheel installation.")
    args = parser.parse_args()
    if args.require_guide:
        if platform.system() != "Linux" or platform.machine() != "x86_64":
            parser.error("guide qualification currently requires Linux x86_64")
        args.require_backend = ["jax", "casadi"]
    import rumoca

    imported = Path(rumoca.__file__).resolve()
    if args.installed_root is not None:
        assert imported.is_relative_to(args.installed_root.resolve()), imported
    print(f"wheel_test: Python {platform.python_version()}, installed rumoca {imported}", flush=True)
    for backend in args.require_backend:
        dependency = importlib.import_module(backend)
        print(f"wheel_test: required {backend} {dependency.__version__}", flush=True)
    tests = list(TESTS)
    if args.require_guide:
        tests.extend(["guide_test.py", "guide_scenario_test.py", "benchmark_test.py"])
    root = Path(__file__).resolve().parent
    for name in tests:
        command = [sys.executable, str(root / name)]
        if name == "export_test.py":
            for backend in args.require_backend:
                command.extend(["--require-backend", backend])
        subprocess.run(command, check=True, timeout=600)
    print(f"wheel_test: {len(tests)} required scripts PASS", flush=True)


if __name__ == "__main__":
    main()
