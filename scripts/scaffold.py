#!/usr/bin/env python3
"""Extract a module's real port list into a lockfile.

Lockfiles are machine-owned artifacts that ensure a upstream port rename shows up as a diff plus a build error.

  python3 scripts/scaffold.py generated/hardfloat/FPADD_8_24.sv
  python3 scripts/scaffold.py generated/            # whole tree
  python3 scripts/scaffold.py cvfpu/src/fpnew_top.sv --library cvfpu

Library is read from the path for elaborated RTL under generated/<library>/. Anything living elsewhere, a submodule especially, needs --library.
"""

from __future__ import annotations

import argparse
import json
import os
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from estimate import preprocess_sv_file  # noqa: E402  shared with the Yosys and ORFS paths

LOCK_DIR = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "descriptors", "_locks")


def _walk(node, want, out):
    if isinstance(node, dict):
        if node.get("type") == want:
            out.append(node)
        for v in node.values():
            _walk(v, want, out)
    elif isinstance(node, list):
        for v in node:
            _walk(v, want, out)
    return out


def _width(range_str):
    # Absent range means a scalar. "31:0" means 32 bits.
    if not range_str:
        return 1
    left, right = range_str.split(":")
    return int(left) - int(right) + 1


def verilator_ports(sv_path, top=None):
    """Return (top_module_name, [{name, dir, width}]) via verilator --json-only."""
    # firtool emits `include "layers-<mod>-Verification.sv" for files it writes separately.
    # The same preprocessing feeds Yosys and ORFS, so the lock describes the RTL that ships.
    content = preprocess_sv_file(sv_path)
    if content is None:
        raise RuntimeError(f"preprocessing failed for {sv_path}")

    with tempfile.TemporaryDirectory() as tmp:
        staged = os.path.join(tmp, os.path.basename(sv_path))
        with open(staged, "w") as fh:
            fh.write(content)

        cmd = ["verilator", "--json-only", "-Wno-fatal", "-Mdir", tmp]
        if top:
            cmd += ["--top-module", top]
        cmd.append(staged)
        proc = subprocess.run(cmd, capture_output=True, text=True, check=False)
        trees = [f for f in os.listdir(tmp) if f.endswith(".tree.json")]
        if not trees:
            raise RuntimeError(f"verilator produced no AST for {sv_path}\n{proc.stderr[:800]}")
        with open(os.path.join(tmp, trees[0])) as fh:
            tree = json.load(fh)

    widths = {}
    for dt in _walk(tree, "BASICDTYPE", []):
        widths[dt["addr"]] = _width(dt.get("range"))

    modules = _walk(tree, "MODULE", [])
    if not modules:
        raise RuntimeError(f"no MODULE node in {sv_path}")
    # Verilator marks the elaborated top; fall back to the first module it kept.
    tops = [m for m in modules if m.get("topModule")] or modules[:1]
    top_mod = tops[0]
    top_name = top_mod.get("origName") or top_mod.get("name")

    ports = []
    for var in _walk(top_mod, "VAR", []):
        direction = var.get("direction")
        if direction not in ("INPUT", "OUTPUT"):
            continue
        ports.append({
            "name": var["name"],
            "dir": "in" if direction == "INPUT" else "out",
            "width": widths.get(var.get("dtypep"), 1),
        })
    # Verilator can visit a port more than once; keep first sighting, preserve order.
    seen, uniq = set(), []
    for p in ports:
        if p["name"] not in seen:
            seen.add(p["name"])
            uniq.append(p)
    return top_name, uniq


def verilator_version():
    out = subprocess.run(["verilator", "--version"], capture_output=True,
                         text=True, check=False).stdout.strip()
    return out or "unknown"


def scaffold_one(sv_path, repo_root, library=None):
    rel = os.path.relpath(sv_path, repo_root)
    parts = rel.replace("\\", "/").split("/")
    if library is None:
        if len(parts) > 2 and parts[0] == "generated":
            library = parts[1]
        else:
            raise RuntimeError(
                f"cannot infer library from {rel}. Pass --library.")
    stem = os.path.splitext(os.path.basename(sv_path))[0]

    top, ports = verilator_ports(sv_path)
    lock = {
        "design": f"{library}/{stem}",
        "library": library,
        "stem": stem,
        "module": top,
        "source": rel,
        "tool": verilator_version(),
        "ports": ports,
    }
    os.makedirs(LOCK_DIR, exist_ok=True)
    out_path = os.path.join(LOCK_DIR, f"{library}__{stem}.json")
    with open(out_path, "w") as fh:
        json.dump(lock, fh, indent=2)
        fh.write("\n")
    return out_path, lock


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("path", help="An .sv file or a directory to walk.")
    ap.add_argument("--library", help="Override the library name. Required outside generated/.")
    ap.add_argument("--quiet", action="store_true")
    args = ap.parse_args()

    repo_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

    targets = []
    if os.path.isdir(args.path):
        for root, _, files in os.walk(args.path):
            targets += [os.path.join(root, f) for f in sorted(files) if f.endswith(".sv")]
    else:
        targets = [args.path]

    if not targets:
        print(f"no .sv files under {args.path}", file=sys.stderr)
        return 1

    failures = []
    for sv in targets:
        try:
            _, lock = scaffold_one(sv, repo_root, args.library)
            if not args.quiet:
                print(f"  {lock['design']:44s} top={lock['module']:22s} {len(lock['ports'])} ports")
        except Exception as exc:
            failures.append((sv, str(exc).splitlines()[0]))

    print(f"\nlocked {len(targets) - len(failures)}/{len(targets)} designs -> descriptors/_locks/")
    for sv, msg in failures:
        print(f"  FAILED {sv}: {msg}", file=sys.stderr)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
