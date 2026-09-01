"""Run verification for one or more designs.

  python3 -m scripts.verification.run --design hardfloat/FPADD_8_24
  python3 -m scripts.verification.run --library hardfloat --library openfloat
  python3 -m scripts.verification.run --design hardfloat/FPADD_8_24 --max-vectors 2000

Each (design, rounding mode, tininess) pair is one simulation and one record. The set of pairs
comes from the design's own capability profile, so a DUT with no rounding port runs once and says
so rather than pretending to sweep.
"""

from __future__ import annotations

import argparse
import json
import os
import sys
import tempfile
import time
from pathlib import Path

from .capabilities import Profile
from .record import RESULTS_DIR
from .stimulus.testfloat import generate

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = ROOT / "generated" / "descriptor_manifest.json"

# Verilator shells out to verilator_includer, a python3 script. cocotb has already exported a
# PYTHONPATH for our interpreter, so a different python3 on PATH aborts the C++ build. Put ours
# first and the two always agree.
os.environ["PATH"] = os.path.dirname(sys.executable) + os.pathsep + os.environ.get("PATH", "")


def load_designs(args):
    if not MANIFEST.exists():
        sys.exit(f"no manifest at {MANIFEST}. Run: python3 scripts/build_manifest.py")
    designs = json.loads(MANIFEST.read_text())["designs"]

    if args.design:
        wanted = set(args.design)
        designs = [d for d in designs if d["design_id"] in wanted]
        missing = wanted - {d["design_id"] for d in designs}
        if missing:
            sys.exit(f"unknown design(s): {', '.join(sorted(missing))}")
    if args.library:
        designs = [d for d in designs if d["library"] in set(args.library)]
    if args.operator:
        designs = [d for d in designs if d["operator"] in set(args.operator)]
    if args.tier:
        designs = [d for d in designs if d["tier"] == args.tier]
    return designs


def build_once(design, build_dir):
    from cocotb_tools.runner import get_runner
    sv = ROOT / "generated" / design["library"] / f"{design['stem']}.sv"
    if not sv.exists():
        sys.exit(f"{sv} missing. Run: sbt \"runMain Generate.GenerateAllTestModules\"")

    # firtool emits layer includes it writes separately; the same preprocessing feeds Yosys.
    from ..estimate import preprocess_sv_file

    build_dir.mkdir(parents=True, exist_ok=True)
    staged = build_dir / sv.name
    staged.write_text(preprocess_sv_file(str(sv)))

    runner = get_runner("verilator")
    runner.build(
        verilog_sources=[staged],
        hdl_toplevel=design["module"],
        build_dir=str(build_dir),
        always=True,
        build_args=["-Wno-fatal", "--trace-structs" if os.environ.get("VCD") else "-Wno-WIDTH"],
    )
    return runner


def run_one(runner, design, rounding, tininess, max_vectors, results_dir):
    stim = design.get("stimulus") or {}
    function = stim.get("testfloat_function")
    if not function:
        return None, f"{design['design_id']}: no stimulus.testfloat_function; skipped"

    vectors = generate(
        function=function, rounding=rounding, tininess=tininess,
        level=stim.get("level", 1), seed=stim.get("seed", 1),
        max_vectors=stim.get("max_vectors"), reference=stim.get("reference", "testfloat"),
    )

    dsn = design["design_id"].replace("/", "__")
    result_path = os.path.join(results_dir, f"{dsn}__{rounding}__{tininess}.json")

    job = {
        "design": design,
        "vectors": vectors.sidecar(),
        "rounding": rounding,
        "tininess": tininess,
        "max_vectors": max_vectors,
        "result_path": result_path,
    }
    # A record left by an earlier sweep must never be mistaken for this run's result.
    if os.path.exists(result_path):
        os.unlink(result_path)

    fd, job_path = tempfile.mkstemp(suffix=".json", prefix="openhwfp_job_")
    with os.fdopen(fd, "w") as fh:
        json.dump(job, fh)

    try:
        runner.test(
            hdl_toplevel=design["module"],
            test_module="scripts.verification.driver",
            # test_dir lands on PYTHONPATH, which is how the driver imports the package.
            test_dir=str(ROOT),
            # cocotb writes JUnit XML; keep it beside the build, not in the repo root.
            results_xml=str(Path(runner.build_dir) / "results.xml"),
            extra_env={"OPENHWFP_JOB": job_path},
        )
        crashed = False
    except (SystemExit, Exception):
        # A mismatch also raises here; the record is written before the driver gives up.
        crashed = True
    finally:
        os.unlink(job_path)

    if os.path.exists(result_path):
        with open(result_path) as fh:
            return json.load(fh), None
    detail = "simulation crashed before writing one" if crashed else "driver produced none"
    return None, f"{design['design_id']} {rounding}/{tininess}: no record ({detail})"


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--design", action="append", help="design_id, repeatable")
    ap.add_argument("--library", action="append", help="library name, repeatable")
    ap.add_argument("--operator", action="append", help="operator name, repeatable")
    ap.add_argument("--tier", type=int, choices=[1, 2])
    ap.add_argument("--max-vectors", type=int, help="cap vectors per run, for smoke tests")
    ap.add_argument("--results-dir", default=RESULTS_DIR)
    ap.add_argument("--build-dir", default="sim_build")
    ap.add_argument("--setup", action="store_true",
                    help="Build SoftFloat and TestFloat first if testfloat_gen is missing.")
    args = ap.parse_args()

    if args.setup:
        from .stimulus.testfloat import ensure_reference_stack
        ensure_reference_stack()

    designs = load_designs(args)
    if not designs:
        sys.exit("no designs matched")

    os.makedirs(args.results_dir, exist_ok=True)
    records, problems = [], []

    for design in designs:
        profile = Profile.from_manifest(design["profile"])
        runs = list(profile.runs())
        print(f"\n=== {design['design_id']}  tier{design['tier']}  "
              f"{design['conformance_level']}  {len(runs)} run(s)")

        build_dir = Path(args.build_dir) / design["design_id"].replace("/", "__")
        try:
            runner = build_once(design, build_dir)
        except Exception as exc:
            problems.append(f"{design['design_id']}: build failed: {exc}")
            continue

        for rounding, tininess in runs:
            t0 = time.time()
            rec, err = run_one(runner, design, rounding, tininess, args.max_vectors, args.results_dir)
            dt = time.time() - t0
            if err:
                problems.append(err)
                print(f"  {rounding}/{tininess:6s} ERROR  {err}")
                continue
            records.append(rec)
            mark = "pass" if rec["status"] == "pass" else "FAIL"
            print(f"  {rounding}/{tininess:6s} {mark}  "
                  f"{rec['checks_performed']} checks, {rec['mismatch_count']} mismatches, "
                  f"flags={rec['flag_check']}, {dt:.1f}s")

    print("\n" + "=" * 78)
    passed = sum(1 for r in records if r["status"] == "pass")
    print(f"{passed}/{len(records)} runs passed")
    for p in problems:
        print(f"  problem: {p}")
    print(f"records -> {args.results_dir}/")
    return 1 if problems or passed != len(records) else 0


if __name__ == "__main__":
    sys.exit(main())
