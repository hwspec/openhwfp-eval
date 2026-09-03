"""The port contract must fail loudly. A silent pass is the one outcome we cannot tolerate.

Each test mutates a real descriptor in a temp copy of the tree and asserts the builder rejects it.
"""

import json
import os
import shutil
import subprocess
import sys

import pytest
import yaml

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# A strict design (rounding-mode and flag ports) and a minimal one (neither). The interesting
# violations run in opposite directions on the two, so both subjects earn their keep.
SUBJECT = "descriptors/hardfloat/add_fp32.yaml"
MINIMAL_SUBJECT = "descriptors/openfloat/add_fp32_pd1.yaml"


def run_builder(tree, *extra):
    proc = subprocess.run(
        [sys.executable, os.path.join(tree, "scripts", "build_manifest.py"), "--check", *extra],
        capture_output=True, text=True, cwd=tree, check=False,
    )
    return proc.returncode, proc.stdout + proc.stderr


@pytest.fixture
def tree(tmp_path):
    dst = tmp_path / "repo"
    for sub in ("descriptors", "scripts"):
        shutil.copytree(os.path.join(ROOT, sub), dst / sub)
    return str(dst)


def mutate(tree, fn, subject=SUBJECT):
    path = os.path.join(tree, subject)
    with open(path) as fh:
        desc = yaml.safe_load(fh)
    fn(desc)
    with open(path, "w") as fh:
        yaml.safe_dump(desc, fh, sort_keys=False)


def test_clean_tree_passes(tree):
    rc, out = run_builder(tree)
    assert rc == 0, out
    assert "designs pass the contract" in out


def test_typo_in_port_name_is_rejected(tree):
    mutate(tree, lambda d: d["ports"]["a"].update(chisel="io.in_A"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "does not have" in out and "io_in_A" in out


def test_wrong_width_is_rejected(tree):
    mutate(tree, lambda d: d["ports"]["a"].update(width=16))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "declared width 16 but is 32" in out


def test_wrong_direction_is_rejected(tree):
    mutate(tree, lambda d: d["ports"]["a"].update(dir="out"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "declared out but is in" in out


def test_claiming_flags_without_a_flags_port_is_rejected(tree):
    """The silent-pass hole: profile says flags are checked, no port carries them."""
    mutate(tree, lambda d: d["profile"].update(exception_flags="ieee5"), MINIMAL_SUBJECT)
    rc, out = run_builder(tree)
    assert rc == 1
    assert "no 'flags' port is mapped" in out


def test_dropping_the_flags_port_while_still_claiming_it_is_rejected(tree):
    """Same hole from the other side: the port goes away, the claim stays.

    Coverage catches this before the profile check does, which is fine. What matters is that the
    message names the signal so the fix is obvious.
    """
    mutate(tree, lambda d: d["ports"].pop("flags"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "io_exceptionFlags" in out


def test_having_a_flags_port_while_claiming_none_is_rejected(tree):
    """An unchecked flag port is a silently narrowed check, so it is also an error."""
    mutate(tree, lambda d: d["profile"].update(exception_flags="none"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "a 'flags' port is mapped" in out


def test_unaccounted_port_is_rejected(tree):
    mutate(tree, lambda d: d["ports"].pop("b"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "unaccounted ports" in out and "io_in_b" in out


def test_multiple_modes_without_a_mode_port_is_rejected(tree):
    def claim_modes(d):
        d["profile"].update(rounding_modes=["rne", "rtz", "rdn"])
    mutate(tree, claim_modes, MINIMAL_SUBJECT)
    rc, out = run_builder(tree)
    assert rc == 1
    assert "no rounding_mode port" in out


def test_reduced_rounding_coverage_needs_no_prose(tree):
    """not_evaluated_reason is documentation, not a gate. Its absence must not fail a build."""
    mutate(tree, lambda d: d["profile"].pop("not_evaluated_reason"), MINIMAL_SUBJECT)
    rc, out = run_builder(tree)
    assert rc == 0, out


def test_unknown_role_is_rejected(tree):
    def add_bogus(d):
        d["ports"]["frobnicate"] = {"chisel": "io.out", "dir": "out", "width": 32}
    mutate(tree, add_bogus)
    rc, out = run_builder(tree)
    assert rc == 1
    assert "unknown role" in out


def test_wrong_module_name_is_rejected(tree):
    mutate(tree, lambda d: d.update(module="NotTheRealModule"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "elaborates to" in out


def test_tier2_operator_without_a_budget_is_rejected(tree):
    # An operator with no row in budgets.py is rejected at tier 2.
    mutate(tree, lambda d: d.update(tier=2, operator="madeup_op"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "budget" in out.lower()


def test_tier2_hardcoded_ulp_budget_is_rejected(tree):
    # Tier-2 budgets are derived per-format from the table; a hand-written one is a contract error.
    mutate(tree, lambda d: (d.update(tier=2, operator="exp"), d["profile"].update(ulp_budget=4.0)))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "ulp_budget" in out


def test_ignore_ports_must_name_real_ports(tree):
    mutate(tree, lambda d: d.update(ignore_ports=["io_does_not_exist"]))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "which the module does not have" in out


class TestClockAndResetNames:
    """Chisel says clock/reset. Nobody else does, so the name is descriptor data and must be real."""

    def test_clock_naming_a_missing_port_is_rejected(self, tree):
        mutate(tree, lambda d: d["sim"].update(clock="clk_i"))
        rc, out = run_builder(tree)
        assert rc == 1
        assert "sim.clock is 'clk_i'" in out

    def test_reset_naming_a_missing_port_is_rejected(self, tree):
        mutate(tree, lambda d: d["sim"].update(reset="rst_n"))
        rc, out = run_builder(tree)
        assert rc == 1
        assert "sim.reset is 'rst_n'" in out

    def test_the_error_suggests_the_real_port(self, tree):
        mutate(tree, lambda d: d["sim"].update(clock="clk"))
        assert "Did you mean ['clock']" in run_builder(tree)[1]

    def test_null_means_the_design_has_none(self, tree):
        """A clockless DUT is legal; it just has to say so instead of naming a phantom."""
        mutate(tree, lambda d: d["sim"].update(clock=None))
        rc, out = run_builder(tree)
        assert rc == 1
        assert "sim.clock" not in out
        assert "unaccounted ports" in out and "clock" in out

    def test_a_renamed_clock_is_not_silently_swallowed(self, tree):
        """Misnaming it must not let the real port slip through as accounted for."""
        mutate(tree, lambda d: (d["sim"].update(clock="clk"),
                                d.update(ignore_ports=["clock"])))
        assert run_builder(tree)[0] == 1


def test_descriptor_needs_generator_or_source(tree):
    mutate(tree, lambda d: d.pop("generator"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "schema" in out


def test_generator_and_source_cannot_both_appear(tree):
    mutate(tree, lambda d: d.update(source="somewhere/else.sv"))
    rc, out = run_builder(tree)
    assert rc == 1
    assert "schema" in out


def test_source_design_is_absent_from_the_plan(tree):
    """Pre-existing RTL is read where it sits. Nothing tries to elaborate it."""
    mutate(tree, lambda d: (d.pop("generator"), d.update(source="cvfpu/src/fpnew_top.sv")))
    rc, out = run_builder(tree, "--plan-only")
    assert rc == 0, out
    assert "67 designs to elaborate" in out


class TestPlanIsNotLockGated:
    """A new design must reach the elaborator before its ports have ever been observed."""

    def test_unlocked_descriptor_still_plans(self, tree):
        os.remove(os.path.join(tree, "descriptors", "_locks", "hardfloat__FPADD_8_24.json"))
        assert run_builder(tree, "--plan-only")[0] == 0

    def test_unlocked_descriptor_fails_the_manifest(self, tree):
        os.remove(os.path.join(tree, "descriptors", "_locks", "hardfloat__FPADD_8_24.json"))
        rc, out = run_builder(tree)
        assert rc == 1
        assert "no lockfile for hardfloat/FPADD_8_24" in out

    def test_no_locks_at_all_still_plans(self, tree):
        shutil.rmtree(os.path.join(tree, "descriptors", "_locks"))
        rc, out = run_builder(tree, "--plan-only")
        assert rc == 0, out
        assert "68 designs to elaborate" in out

    def test_a_schema_error_stops_the_plan(self, tree):
        """Garbage in one descriptor must not elaborate the other 70 against a partial plan."""
        mutate(tree, lambda d: d.update(format={"exp_bits": 8}))
        assert run_builder(tree, "--plan-only")[0] == 1


def test_conformance_level_is_recorded(tree):
    """Reduced capability is data, not an omission."""
    proc = subprocess.run(
        [sys.executable, os.path.join(tree, "scripts", "build_manifest.py")],
        capture_output=True, text=True, cwd=tree, check=False,
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    with open(os.path.join(tree, "generated", "descriptor_manifest.json")) as fh:
        manifest = json.load(fh)
    levels = {d["conformance_level"] for d in manifest["designs"]}
    assert levels <= {"strict", "reduced", "minimal"}
    for d in manifest["designs"]:
        if d["profile"]["exception_flags"] == "none":
            assert d["conformance_level"] != "strict"
