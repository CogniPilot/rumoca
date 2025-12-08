#!/usr/bin/env python3
"""
Bundle Modelica Standard Library as JSON for WASM use.

This script reads all .mo files from the MSL directory and creates a JSON file
that maps relative file paths to their contents.

Usage:
    ./scripts/bundle-msl.py /path/to/ModelicaStandardLibrary output.json

The output can be loaded in JavaScript and passed to compile_with_libraries().
"""

import json
import os
import sys
from pathlib import Path


def bundle_msl(msl_path: str, output_path: str, packages: list[str] | None = None):
    """Bundle MSL .mo files into a JSON file.

    Args:
        msl_path: Path to the ModelicaStandardLibrary directory
        output_path: Output JSON file path
        packages: Optional list of packages to include (e.g., ['Modelica', 'Complex'])
                 If None, includes all packages except test packages
    """
    msl_dir = Path(msl_path)
    if not msl_dir.exists():
        print(f"Error: MSL directory not found: {msl_path}")
        sys.exit(1)

    # Default packages to include
    if packages is None:
        packages = ['Modelica', 'ModelicaServices', 'Complex']

    files = {}
    total_size = 0
    file_count = 0

    for package in packages:
        package_path = msl_dir / package

        # Handle single-file packages (like Complex.mo)
        if package_path.with_suffix('.mo').exists():
            mo_file = package_path.with_suffix('.mo')
            rel_path = mo_file.relative_to(msl_dir)
            content = mo_file.read_text(encoding='utf-8')
            files[str(rel_path)] = content
            total_size += len(content)
            file_count += 1
            print(f"  Added {rel_path}")
            continue

        if not package_path.exists():
            print(f"Warning: Package not found: {package}")
            continue

        # Walk the package directory
        for mo_file in package_path.rglob('*.mo'):
            # Skip test files
            if 'Test' in str(mo_file) or 'Obsolete' in str(mo_file):
                continue

            rel_path = mo_file.relative_to(msl_dir)
            try:
                content = mo_file.read_text(encoding='utf-8')
                files[str(rel_path)] = content
                total_size += len(content)
                file_count += 1
            except Exception as e:
                print(f"Warning: Failed to read {mo_file}: {e}")

    print(f"\nBundled {file_count} files, {total_size / 1024:.1f} KB")

    # Write JSON output
    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(files, f, separators=(',', ':'))  # Compact JSON

    output_size = os.path.getsize(output_path)
    print(f"Output: {output_path} ({output_size / 1024:.1f} KB)")

    return files


def create_minimal_bundle(msl_path: str, output_path: str):
    """Create a minimal bundle with just essential types.

    This includes:
    - Modelica.SIunits (now Modelica.Units)
    - Modelica.Constants
    - Modelica.Math (basic functions)
    """
    msl_dir = Path(msl_path)
    files = {}

    # Essential files to include
    essential_paths = [
        'Complex.mo',
        'Modelica/package.mo',
        'Modelica/Constants.mo',
        'Modelica/Math/package.mo',
        'Modelica/Units/package.mo',
        'Modelica/Units/SI.mo',
        'Modelica/Units/NonSI.mo',
        'Modelica/Units/Conversions.mo',
        'ModelicaServices/package.mo',
    ]

    for rel_path in essential_paths:
        full_path = msl_dir / rel_path
        if full_path.exists():
            files[rel_path] = full_path.read_text(encoding='utf-8')
            print(f"  Added {rel_path}")
        else:
            print(f"  Skipped {rel_path} (not found)")

    print(f"\nMinimal bundle: {len(files)} files")

    with open(output_path, 'w', encoding='utf-8') as f:
        json.dump(files, f, separators=(',', ':'))

    output_size = os.path.getsize(output_path)
    print(f"Output: {output_path} ({output_size / 1024:.1f} KB)")

    return files


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print(__doc__)
        print("\nExamples:")
        print("  Full MSL:    ./scripts/bundle-msl.py /path/to/MSL pkg/msl.json")
        print("  Minimal:     ./scripts/bundle-msl.py /path/to/MSL pkg/msl-minimal.json --minimal")
        sys.exit(1)

    msl_path = sys.argv[1]
    output_path = sys.argv[2]
    minimal = '--minimal' in sys.argv

    print(f"Bundling MSL from: {msl_path}")
    print(f"Output: {output_path}")
    print(f"Mode: {'minimal' if minimal else 'full'}\n")

    if minimal:
        create_minimal_bundle(msl_path, output_path)
    else:
        bundle_msl(msl_path, output_path)
