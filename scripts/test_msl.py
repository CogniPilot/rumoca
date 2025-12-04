#!/usr/bin/env python3
"""
Modelica Standard Library (MSL) Compilation Test Script

This script attempts to compile every model in the Modelica Standard Library
using rumoca and generates detailed statistics and error reports.

Usage:
    python test_msl.py --msl-path /path/to/MSL
    python test_msl.py --msl-path /path/to/MSL --limit 100
    python test_msl.py --msl-path /path/to/MSL --output results.json
    python test_msl.py --msl-path /path/to/MSL --package Modelica.Mechanics
"""

import argparse
import json
import os
import re
import subprocess
import sys
import time
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Optional
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading


@dataclass
class ModelResult:
    """Result of attempting to compile a single model."""
    model_name: str
    file_path: str
    success: bool
    error_message: Optional[str] = None
    error_category: Optional[str] = None
    compile_time_ms: float = 0.0
    is_balanced: Optional[bool] = None


@dataclass
class TestStats:
    """Aggregated statistics for MSL testing."""
    total_models: int = 0
    passed: int = 0
    failed: int = 0
    parse_errors: int = 0
    flatten_errors: int = 0
    dae_errors: int = 0
    balance_errors: int = 0
    other_errors: int = 0
    total_time_s: float = 0.0
    errors_by_category: dict = field(default_factory=dict)

    @property
    def pass_rate(self) -> float:
        if self.total_models == 0:
            return 0.0
        return (self.passed / self.total_models) * 100.0

    def categorize_error(self, model_name: str, error: str) -> str:
        """Categorize an error and update counts."""
        error_lower = error.lower()

        if "parse" in error_lower or "syntax" in error_lower or "unexpected" in error_lower:
            category = "parse_errors"
            self.parse_errors += 1
        elif "flatten" in error_lower or "resolve" in error_lower or "undefined" in error_lower:
            category = "flatten_errors"
            self.flatten_errors += 1
        elif "dae" in error_lower or "structural" in error_lower:
            category = "dae_errors"
            self.dae_errors += 1
        elif "balance" in error_lower or "under-determined" in error_lower or "over-determined" in error_lower:
            category = "balance_errors"
            self.balance_errors += 1
        else:
            category = "other_errors"
            self.other_errors += 1

        if category not in self.errors_by_category:
            self.errors_by_category[category] = []
        self.errors_by_category[category].append({
            "model": model_name,
            "error": error[:500]  # Truncate long errors
        })

        return category


def find_mo_files(directory: Path, skip_resources: bool = True) -> list[Path]:
    """Find all .mo files in a directory recursively."""
    mo_files = []

    for root, dirs, files in os.walk(directory):
        # Skip hidden directories and Resources
        if skip_resources:
            dirs[:] = [d for d in dirs if not d.startswith('.') and d != 'Resources']

        for file in files:
            if file.endswith('.mo'):
                mo_files.append(Path(root) / file)

    return mo_files


def extract_class_names_from_file(file_path: Path) -> list[str]:
    """Extract top-level class/model names from a Modelica file using regex."""
    try:
        content = file_path.read_text(encoding='utf-8', errors='replace')
    except Exception as e:
        return []

    # Pattern to match class definitions: model/class/block/connector/record/function/package/type Name
    pattern = r'^\s*(?:partial\s+)?(?:encapsulated\s+)?(model|class|block|connector|record|function|package|type)\s+(\w+)'

    names = []
    for match in re.finditer(pattern, content, re.MULTILINE):
        class_type, name = match.groups()
        # Skip package.mo files as they define the package itself
        if file_path.name == 'package.mo':
            continue
        names.append(name)

    return names


def get_qualified_name(file_path: Path, class_name: str, msl_root: Path) -> str:
    """Get the fully qualified Modelica name for a class."""
    # Get relative path from MSL root
    try:
        rel_path = file_path.relative_to(msl_root)
    except ValueError:
        return class_name

    # Convert path to qualified name
    parts = list(rel_path.parts[:-1])  # Exclude the filename

    # Handle package structure
    if file_path.name == 'package.mo':
        # This defines the package itself
        return '.'.join(parts)
    else:
        # This is a class in the package
        file_stem = file_path.stem
        if parts and file_stem == parts[-1]:
            # File name matches parent - avoid duplication
            return '.'.join(parts)
        return '.'.join(parts + [class_name])


def compile_model_with_rumoca(
    file_path: Path,
    model_name: str,
    msl_path: Path,
) -> ModelResult:
    """Compile a single model using rumoca CLI."""
    start_time = time.time()

    try:
        # Use rumoca CLI with JSON output to get structured results
        result = subprocess.run(
            [
                'rumoca',
                str(file_path),
                '--model', model_name,
                '--json'
            ],
            capture_output=True,
            text=True,
            timeout=30,  # 30 second timeout per model
            env={**os.environ, 'MODELICAPATH': str(msl_path)}
        )

        compile_time = (time.time() - start_time) * 1000

        if result.returncode == 0:
            # Try to parse JSON output to check balance
            is_balanced = None
            try:
                output = json.loads(result.stdout)
                # Check if model is balanced from the output
                if 'structure' in output:
                    n_eq = output['structure'].get('n_equations', 0)
                    n_unk = output['structure'].get('n_states', 0) + output['structure'].get('n_algebraic', 0)
                    is_balanced = n_eq == n_unk
            except (json.JSONDecodeError, KeyError):
                pass

            return ModelResult(
                model_name=model_name,
                file_path=str(file_path),
                success=True,
                compile_time_ms=compile_time,
                is_balanced=is_balanced
            )
        else:
            error_msg = result.stderr or result.stdout or "Unknown error"
            return ModelResult(
                model_name=model_name,
                file_path=str(file_path),
                success=False,
                error_message=error_msg.strip(),
                compile_time_ms=compile_time
            )

    except subprocess.TimeoutExpired:
        return ModelResult(
            model_name=model_name,
            file_path=str(file_path),
            success=False,
            error_message="Compilation timeout (>30s)",
            error_category="timeout"
        )
    except Exception as e:
        return ModelResult(
            model_name=model_name,
            file_path=str(file_path),
            success=False,
            error_message=str(e),
            error_category="internal_error"
        )


def run_msl_tests(
    msl_path: Path,
    limit: Optional[int] = None,
    package_filter: Optional[str] = None,
    parallel: int = 1,
    verbose: bool = False
) -> tuple[TestStats, list[ModelResult]]:
    """Run MSL compilation tests."""
    stats = TestStats()
    results: list[ModelResult] = []
    start_time = time.time()

    # Find the Modelica package
    modelica_dir = msl_path / 'Modelica'
    if not modelica_dir.exists():
        print(f"Error: Modelica package not found at {modelica_dir}")
        return stats, results

    print(f"Scanning MSL at: {msl_path}")

    # Find all .mo files
    if package_filter:
        # Convert package name to path
        package_path = modelica_dir / package_filter.replace('Modelica.', '').replace('.', '/')
        if package_path.exists():
            mo_files = find_mo_files(package_path)
        else:
            # Try as a subdirectory of Modelica
            package_path = modelica_dir / package_filter.replace('.', '/')
            mo_files = find_mo_files(package_path) if package_path.exists() else []
    else:
        mo_files = find_mo_files(modelica_dir)

    print(f"Found {len(mo_files)} .mo files")

    # Collect all models to test
    models_to_test: list[tuple[Path, str]] = []

    for file_path in mo_files:
        class_names = extract_class_names_from_file(file_path)
        for name in class_names:
            models_to_test.append((file_path, name))
            if limit and len(models_to_test) >= limit:
                break
        if limit and len(models_to_test) >= limit:
            break

    print(f"Found {len(models_to_test)} models to test")

    # Progress tracking
    processed = 0
    lock = threading.Lock()

    def process_model(args):
        nonlocal processed
        file_path, model_name = args
        result = compile_model_with_rumoca(file_path, model_name, msl_path)

        with lock:
            processed += 1
            if result.success:
                print('.', end='', flush=True)
            else:
                print('F', end='', flush=True)

            if processed % 50 == 0:
                print(f' [{processed}/{len(models_to_test)}]')

        return result

    # Run tests
    if parallel > 1:
        with ThreadPoolExecutor(max_workers=parallel) as executor:
            future_to_model = {executor.submit(process_model, m): m for m in models_to_test}
            for future in as_completed(future_to_model):
                results.append(future.result())
    else:
        for model in models_to_test:
            results.append(process_model(model))

    print()

    # Compute statistics
    stats.total_time_s = time.time() - start_time
    for result in results:
        stats.total_models += 1
        if result.success:
            stats.passed += 1
        else:
            stats.failed += 1
            if result.error_message:
                result.error_category = stats.categorize_error(
                    result.model_name,
                    result.error_message
                )

    return stats, results


def print_summary(stats: TestStats, results: list[ModelResult], show_failures: int = 20):
    """Print a summary of test results."""
    print()
    print("=" * 60)
    print("               MSL COMPILATION SUMMARY")
    print("=" * 60)
    print()
    print(f"Total Models:     {stats.total_models}")
    print(f"Passed:           {stats.passed} ({stats.pass_rate:.1f}%)")
    print(f"Failed:           {stats.failed} ({100.0 - stats.pass_rate:.1f}%)")
    print()
    print("Error Breakdown:")
    print(f"  Parse errors:   {stats.parse_errors}")
    print(f"  Flatten errors: {stats.flatten_errors}")
    print(f"  DAE errors:     {stats.dae_errors}")
    print(f"  Balance errors: {stats.balance_errors}")
    print(f"  Other errors:   {stats.other_errors}")
    print()
    print(f"Total Time:       {stats.total_time_s:.2f}s")
    print("=" * 60)

    # Print first N failures
    failures = [r for r in results if not r.success][:show_failures]
    if failures:
        print(f"\nFirst {len(failures)} failures:")
        print("-" * 60)
        for failure in failures:
            print(f"Model: {failure.model_name}")
            print(f"File:  {failure.file_path}")
            if failure.error_message:
                # Truncate long errors
                error = failure.error_message
                if len(error) > 200:
                    error = error[:200] + "..."
                print(f"Error: {error}")
            print()


def write_results(results: list[ModelResult], stats: TestStats, output_path: Path):
    """Write detailed results to a JSON file."""
    output = {
        "summary": {
            "total_models": stats.total_models,
            "passed": stats.passed,
            "failed": stats.failed,
            "pass_rate": stats.pass_rate,
            "parse_errors": stats.parse_errors,
            "flatten_errors": stats.flatten_errors,
            "dae_errors": stats.dae_errors,
            "balance_errors": stats.balance_errors,
            "other_errors": stats.other_errors,
            "total_time_s": stats.total_time_s,
        },
        "errors_by_category": stats.errors_by_category,
        "results": [asdict(r) for r in results]
    }

    with open(output_path, 'w') as f:
        json.dump(output, f, indent=2)

    print(f"\nResults written to: {output_path}")


def main():
    parser = argparse.ArgumentParser(
        description="Test rumoca compilation of the Modelica Standard Library"
    )
    parser.add_argument(
        "--msl-path",
        type=Path,
        required=True,
        help="Path to MSL root (directory containing 'Modelica' folder)"
    )
    parser.add_argument(
        "--limit",
        type=int,
        default=None,
        help="Limit number of models to test (for quick tests)"
    )
    parser.add_argument(
        "--package",
        type=str,
        default=None,
        help="Only test models in a specific package (e.g., 'Mechanics.Rotational')"
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help="Output JSON file for detailed results"
    )
    parser.add_argument(
        "--parallel",
        type=int,
        default=1,
        help="Number of parallel compilation threads"
    )
    parser.add_argument(
        "--show-failures",
        type=int,
        default=20,
        help="Number of failures to show in summary"
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="Verbose output"
    )

    args = parser.parse_args()

    if not args.msl_path.exists():
        print(f"Error: MSL path does not exist: {args.msl_path}")
        sys.exit(1)

    stats, results = run_msl_tests(
        msl_path=args.msl_path,
        limit=args.limit,
        package_filter=args.package,
        parallel=args.parallel,
        verbose=args.verbose
    )

    print_summary(stats, results, args.show_failures)

    if args.output:
        write_results(results, stats, args.output)

    # Exit with error code if pass rate is 0
    if stats.passed == 0 and stats.total_models > 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
