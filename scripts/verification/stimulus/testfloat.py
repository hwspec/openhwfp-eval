"""Materialize testfloat_gen output into vector files.

Files rather than a pipe so a run is byte reproducible, archivable next to its results, and
diffable when someone claims the stimulus changed. The sidecar records everything needed to
regenerate the file, and the sha256 proves it was not edited by hand.
"""

from __future__ import annotations

import hashlib
import json
import os
import shutil
import subprocess
from dataclasses import dataclass, asdict
from typing import Optional

from ..capabilities import ROUNDING, TININESS

DEFAULT_DIR = "vectors"


@dataclass
class VectorSet:
    path: str
    function: str
    rounding: str
    tininess: str
    level: int
    seed: int
    count: int
    operands: int
    sha256: str
    generator: str
    truncated_from: Optional[int] = None
    reference: str = "testfloat"

    def rows(self):
        """Yield (operands, expected_result, expected_flags) as ints.

        An identity set is a bare value stream, so the value is both operand and expectation.
        """
        with open(self.path) as fh:
            for line in fh:
                cols = line.split()
                if not cols:
                    continue
                values = [int(c, 16) for c in cols]
                if self.reference == "identity":
                    yield values[:1], values[0], 0
                    continue
                if len(cols) < 2:
                    continue
                yield values[:-2], values[-2], values[-1]

    def sidecar(self) -> dict:
        d = asdict(self)
        d["path"] = os.path.relpath(self.path)
        return d


def _which_gen() -> str:
    """PATH first, then the vendored build. Nobody needs to export anything."""
    exe = shutil.which("testfloat_gen")
    if exe:
        return exe
    root = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
    local = os.path.join(root, "berkeley-testfloat-3", "build", "Linux-x86_64-GCC", "testfloat_gen")
    if os.path.exists(local):
        return local
    raise RuntimeError(
        "testfloat_gen not found. Build it with:  bash scripts/setup_verification.sh")


def ensure_reference_stack() -> str:
    """Build SoftFloat and TestFloat if they are missing. Only ever called behind --setup.

    Not automatic: it compiles C for half a minute, and by the time this module imports you are
    already inside the venv the setup script would have created, so doing it implicitly would be
    both surprising and too late.
    """
    try:
        return _which_gen()
    except RuntimeError:
        pass
    root = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", "..", ".."))
    script = os.path.join(root, "scripts", "setup_verification.sh")
    print(f"testfloat_gen missing; running {os.path.relpath(script, root)}")
    proc = subprocess.run(["bash", script], cwd=root, check=False)
    if proc.returncode != 0:
        raise RuntimeError("setup_verification.sh failed; see its output above")
    return _which_gen()


def _version(exe: str) -> str:
    out = subprocess.run([exe], capture_output=True, text=True, check=False)
    for line in (out.stdout + out.stderr).splitlines():
        if "Release" in line or "TestFloat" in line:
            return line.strip()
    return "testfloat_gen"


def generate(function: str, rounding: str = "rne", tininess: str = "after", level: int = 1,
             seed: int = 1, max_vectors: Optional[int] = None, reference: str = "testfloat",
             out_dir: str = DEFAULT_DIR, force: bool = False) -> VectorSet:
    """Build (or reuse) the vector file for one function and control setting."""
    if rounding not in ROUNDING:
        raise ValueError(f"unknown rounding mode '{rounding}'")
    if tininess not in TININESS:
        raise ValueError(f"unknown tininess setting '{tininess}'")

    exe = _which_gen()
    os.makedirs(out_dir, exist_ok=True)
    stem = f"{function}__{rounding}__{tininess}__L{level}__s{seed}"
    if reference != "testfloat":
        stem += f"__{reference}"
    if max_vectors:
        stem += f"__n{max_vectors}"
    path = os.path.join(out_dir, stem + ".txt")
    meta_path = path + ".json"

    if os.path.exists(path) and os.path.exists(meta_path) and not force:
        with open(meta_path) as mfh:
            meta = json.load(mfh)
        if meta.get("sha256") == _sha256(path) and meta.get("reference", "testfloat") == reference:
            meta["path"] = path
            return VectorSet(**meta)

    cmd = [exe, "-level", str(level), "-seed", str(seed),
           ROUNDING[rounding][0], TININESS[tininess][0], function]
    with open(path, "w") as fh:
        proc = subprocess.run(cmd, stdout=fh, stderr=subprocess.PIPE, text=True, check=False)
    if proc.returncode != 0:
        os.unlink(path)
        raise RuntimeError(f"testfloat_gen failed for {function}: {proc.stderr.strip()[:400]}")

    total = _line_count(path)
    truncated_from = None
    if max_vectors and total > max_vectors:
        # f32_mulAdd at level 1 is 6.1 million rows. Deterministic head, not a random sample,
        # so the same descriptor always runs the same vectors.
        _truncate(path, max_vectors)
        truncated_from = total
        total = max_vectors

    with open(path) as fh:
        first = fh.readline().split()
    operands = 1 if reference == "identity" else max(len(first) - 2, 0)

    vs = VectorSet(
        path=path, function=function, rounding=rounding, tininess=tininess, level=level,
        seed=seed, count=total, operands=operands, sha256=_sha256(path),
        generator=_version(exe), truncated_from=truncated_from, reference=reference,
    )
    with open(meta_path, "w") as fh:
        json.dump(vs.sidecar(), fh, indent=2)
        fh.write("\n")
    return vs


def _sha256(path: str) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def _line_count(path: str) -> int:
    with open(path, "rb") as fh:
        return sum(chunk.count(b"\n") for chunk in iter(lambda: fh.read(1 << 20), b""))


def _truncate(path: str, keep: int) -> None:
    tmp = path + ".tmp"
    with open(path) as src, open(tmp, "w") as dst:
        for i, line in enumerate(src):
            if i >= keep:
                break
            dst.write(line)
    os.replace(tmp, path)
