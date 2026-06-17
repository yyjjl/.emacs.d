#!/usr/bin/env python3
"""Generate a pyrightconfig.json tailored to the real project layout.

Behavior:
  * Discover Python source roots under PROJECT_ROOT, excluding venvs / caches / vendored dirs.
  * Detect the Python interpreter version (prefers the project's .venv if present).
  * Detect the host platform (Darwin / Linux / Windows).
  * Emit a pyrightconfig.json with `include`, `exclude`, `pythonPlatform`, and per-source-root
    `executionEnvironments`.

Usage:
  python3 generate_pyrightconfig.py --project-root /abs/path [--venv /abs/path/.venv]
                                    [--output /abs/path/pyrightconfig.json] [--force]

If the output file already exists, the script prints a diff and aborts unless --force is given.
"""

from __future__ import annotations

import argparse
import json
import os
import platform
import subprocess
import sys
from pathlib import Path
from typing import List, Optional

EXCLUDE_DIRS = {".venv", "venv", "node_modules", "__pycache__", "build", "dist", ".git", "tmp", ".tox", ".mypy_cache", ".pytest_cache"}
EXCLUDE_GLOBS = ["**/.venv", "**/node_modules", "**/tmp", "**/__pycache__"]


def discover_source_roots(project_root: Path) -> List[Path]:
    """Return a list of directories that look like Python source roots.

    Heuristic:
      1. Walk the tree, skipping EXCLUDE_DIRS.
      2. Collect every directory that directly contains at least one .py file.
      3. Prefer the topmost directory in each chain that contains __init__.py
         (i.e. find package roots, not every sub-package).
      4. If no packages are found, fall back to the topmost dirs that contain .py files.
    """
    py_dirs: set[Path] = set()
    pkg_dirs: set[Path] = set()

    for dirpath, dirnames, filenames in os.walk(project_root):
        # prune excluded dirs in place
        dirnames[:] = [d for d in dirnames if d not in EXCLUDE_DIRS and not d.startswith(".")]
        rel = Path(dirpath)
        has_py = any(f.endswith(".py") for f in filenames)
        if has_py:
            py_dirs.add(rel)
            if "__init__.py" in filenames:
                pkg_dirs.add(rel)

    # If we found packages, walk upward to find the package-root parents.
    if pkg_dirs:
        roots: set[Path] = set()
        for pkg in pkg_dirs:
            # Walk up while the parent also has __init__.py (still inside the package).
            cur = pkg
            while (cur.parent / "__init__.py").exists() and cur.parent != project_root:
                cur = cur.parent
            roots.add(cur.parent)  # the directory that *contains* the top package
        return sorted({r for r in roots if r != project_root} or {project_root})

    # No __init__.py anywhere — fall back: keep only the topmost py-bearing dirs.
    sorted_dirs = sorted(py_dirs, key=lambda p: len(p.parts))
    topmost: List[Path] = []
    for d in sorted_dirs:
        if not any(str(d).startswith(str(t) + os.sep) for t in topmost):
            topmost.append(d)
    return topmost or [project_root]


def detect_python_version(venv: Optional[Path]) -> str:
    """Return Python version as 'major.minor'. Prefers the venv's interpreter."""
    candidates: List[str] = []
    if venv is not None:
        # Both POSIX and Windows layouts.
        candidates.append(str(venv / "bin" / "python"))
        candidates.append(str(venv / "Scripts" / "python.exe"))
    candidates.append(sys.executable or "python3")
    candidates.append("python3")

    for cand in candidates:
        try:
            out = subprocess.check_output(
                [cand, "-c", "import sys; print(f'{sys.version_info.major}.{sys.version_info.minor}')"],
                stderr=subprocess.DEVNULL,
                text=True,
            ).strip()
            if out:
                return out
        except (OSError, subprocess.CalledProcessError):
            continue
    # Last-resort fallback to the version running this script.
    return f"{sys.version_info.major}.{sys.version_info.minor}"


def detect_platform() -> str:
    system = platform.system()
    return {"Darwin": "Darwin", "Linux": "Linux", "Windows": "Windows"}.get(system, "Linux")


def build_config(project_root: Path, venv: Optional[Path]) -> dict:
    roots = discover_source_roots(project_root)
    rels = [str(r.relative_to(project_root)) for r in roots]
    py_version = detect_python_version(venv)
    plat = detect_platform()

    return {
        "include": rels,
        "exclude": EXCLUDE_GLOBS,
        "pythonPlatform": plat,
        "executionEnvironments": [
            {
                "root": rel.rstrip("/") + "/",
                "pythonVersion": py_version,
                "extraPaths": [],
            }
            for rel in rels
        ],
    }


def write_config(output: Path, config: dict, force: bool) -> None:
    if output.exists() and not force:
        existing = output.read_text()
        new = json.dumps(config, indent=2) + "\n"
        if existing.strip() == new.strip():
            print(f"[ok] {output} already up to date.", file=sys.stderr)
            return
        print(f"[abort] {output} already exists and differs from the generated config.", file=sys.stderr)
        print("       Pass --force to overwrite, or merge manually.", file=sys.stderr)
        print("--- existing ---", file=sys.stderr)
        print(existing, file=sys.stderr)
        print("--- generated ---", file=sys.stderr)
        print(new, file=sys.stderr)
        sys.exit(2)
    output.write_text(json.dumps(config, indent=2) + "\n")
    print(f"[wrote] {output}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--project-root", required=True, type=Path, help="Absolute path to the project root.")
    parser.add_argument("--venv", type=Path, default=None, help="Path to the project's virtualenv (optional).")
    parser.add_argument("--output", type=Path, default=None, help="Output file (default: <project-root>/pyrightconfig.json).")
    parser.add_argument("--force", action="store_true", help="Overwrite an existing pyrightconfig.json without prompting.")
    parser.add_argument("--print-only", action="store_true", help="Print the generated config to stdout and exit.")
    args = parser.parse_args()

    project_root: Path = args.project_root.resolve()
    if not project_root.is_dir():
        parser.error(f"--project-root does not exist or is not a directory: {project_root}")

    venv: Optional[Path] = args.venv.resolve() if args.venv else None
    if venv is not None and not venv.exists():
        print(f"[warn] --venv path does not exist: {venv}. Falling back to system python.", file=sys.stderr)
        venv = None

    output: Path = (args.output.resolve() if args.output else project_root / "pyrightconfig.json")

    config = build_config(project_root, venv)

    if args.print_only:
        print(json.dumps(config, indent=2))
        return

    write_config(output, config, force=args.force)


if __name__ == "__main__":
    main()
