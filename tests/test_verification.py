"""Comparator and format semantics.

These encode the equivalence rules. If someone loosens one by accident, a run that should fail
starts passing, which is the failure mode this whole layer exists to prevent.
"""

import pytest

from scripts.verification.capabilities import FLAG_BITS, ROUNDING, TININESS, Profile
from scripts.verification.driver import _pin
from scripts.scaffold import contract, drift
from scripts.verification.compare import (Comparator, categorize, compare_flags, compare_value,
                                          describe, in_scope, ulp_bucket)
from scripts.verification.formats import FpFormat

FP32 = FpFormat(8, 24, "fp32")
FP16 = FpFormat(5, 11, "fp16")
FP64 = FpFormat(11, 53, "fp64")

LAX = Profile(["rne"], "none", "none")
STRICT = Profile(["rne", "rtz", "rdn", "rup", "rna", "rto"], "port", "ieee5",
                 tininess=["before", "after"], nan_payload="ieee", signed_zero="ieee")
TIER2 = Profile(["rne"], "none", "none", ulp_budget=2.0)


class TestFormats:
    @pytest.mark.parametrize("fmt,width,bias", [(FP16, 16, 15), (FP32, 32, 127), (FP64, 64, 1023)])
    def test_geometry(self, fmt, width, bias):
        assert fmt.width == width
        assert fmt.bias == bias
        assert fmt.stored_mantissa_bits == fmt.sig_bits - 1

    @pytest.mark.parametrize("bits,kind", [
        (0x7FC00000, "nan"), (0x7F800001, "nan"),
        (0x7F800000, "inf"), (0xFF800000, "inf"),
        (0x00000000, "zero"), (0x80000000, "zero"),
        (0x00000001, "subnormal"), (0x007FFFFF, "subnormal"),
        (0x00800000, "normal"), (0x3F800000, "normal"),
    ])
    def test_classify(self, bits, kind):
        assert FP32.classify(bits) == kind

    def test_ulp_of_adjacent_values_is_one(self):
        assert FP32.ulp_distance(0x3F800000, 0x3F800001) == 1.0

    def test_ulp_across_zero_counts_both_sides(self):
        assert FP32.ulp_distance(0x00000001, 0x80000001) == 2.0

    def test_ulp_to_infinity_is_infinite(self):
        assert FP32.ulp_distance(0x3F800000, 0x7F800000) == float("inf")


class TestValueComparison:
    def test_identical_bits_always_pass(self):
        for profile in (LAX, STRICT, TIER2):
            assert compare_value(0x3F800000, 0x3F800000, FP32, profile, 1).ok

    def test_nan_payload_ignored_unless_claimed(self):
        assert compare_value(0x7FC00000, 0x7FC00001, FP32, LAX, 1).ok
        assert not compare_value(0x7FC00000, 0x7FC00001, FP32, STRICT, 1).ok

    def test_nan_against_finite_always_fails(self):
        for profile in (LAX, STRICT):
            assert not compare_value(0x7FC00000, 0x3F800000, FP32, profile, 1).ok
            assert not compare_value(0x3F800000, 0x7FC00000, FP32, profile, 1).ok

    def test_zero_sign_ignored_unless_claimed(self):
        assert compare_value(0x00000000, 0x80000000, FP32, LAX, 1).ok
        assert not compare_value(0x00000000, 0x80000000, FP32, STRICT, 1).ok

    def test_tier1_is_bit_exact_even_one_ulp_out(self):
        v = compare_value(0x3F800000, 0x3F800001, FP32, LAX, 1)
        assert not v.ok
        assert v.ulp == 1.0

    def test_tier2_honours_the_budget(self):
        assert compare_value(0x3F800000, 0x3F800002, FP32, TIER2, 2).ok
        assert not compare_value(0x3F800000, 0x3F800005, FP32, TIER2, 2).ok

    def test_wrong_infinity_never_passes_on_a_budget(self):
        assert not compare_value(0x7F800000, 0x3F800000, FP32, TIER2, 2).ok


class TestFlagComparison:
    def test_flags_skipped_when_not_claimed(self):
        assert compare_flags(0x01, 0x1F, LAX).ok

    def test_flags_compared_when_claimed(self):
        assert compare_flags(0x03, 0x03, STRICT).ok
        assert not compare_flags(0x01, 0x03, STRICT).ok

    def test_missing_flags_fail_a_claiming_profile(self):
        """A claimed check with nothing to check must not pass."""
        assert not compare_flags(None, 0x01, STRICT).ok

    def test_flag_bit_order_matches_testfloat(self):
        assert FLAG_BITS == ("inexact", "underflow", "overflow", "divzero", "invalid")


class TestProfileRuns:
    def test_strict_profile_sweeps_modes_and_tininess(self):
        assert len(list(STRICT.runs())) == 12

    def test_reduced_profile_runs_once(self):
        assert list(LAX.runs()) == [("rne", "after")]

    def test_every_mode_maps_to_a_testfloat_switch_and_an_encoding(self):
        for mode, (switch, value) in ROUNDING.items():
            assert switch.startswith("-r")
            assert 0 <= value <= 7
        assert set(TININESS) == {"before", "after"}


class TestComparatorAccounting:
    def test_counts_and_records(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        assert c.check(0, [0x3F800000, 0x00000000], 0x3F800000, 0x3F800000)
        assert not c.check(1, [0x3F800000, 0x00000001], 0x3F800000, 0x3F800001)
        assert c.checks == 2
        assert c.mismatches == 1
        assert c.recorded[0].index == 1
        assert c.max_ulp == 1.0

    def test_special_case_coverage_is_tallied(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        c.check(0, [0x7FC00000, 0x00000001], 0x0, 0x0)
        c.check(1, [0x80000000, 0x7F800000], 0x0, 0x0)
        assert c.coverage["nan"] == 1
        assert c.coverage["subnormal"] == 1
        assert c.coverage["signed_zero"] == 1
        assert c.coverage["inf"] == 1

    def test_recorded_mismatches_are_capped(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1, max_recorded=3)
        for i in range(50):
            c.check(i, [0x0, 0x0], 0x3F800000, 0x3F800001)
        assert c.mismatches == 50
        assert len(c.recorded) == 3


FTZ = Profile(["rne"], "elaboration", "none", subnormals="flushed",
              not_evaluated_reason="disableSubnormal is true for every Rial format")

MIN_SUBNORMAL_32 = 0x00000001
MAX_SUBNORMAL_32 = 0x007FFFFF
ONE_32 = 0x3F800000


class TestFlushToZeroScope:
    """A flush-to-zero design has no opinion about subnormals. Holding it to IEEE measures nothing.

    These vectors must be excluded and counted, never silently passed and never counted as
    failures.
    """

    def test_ieee_profile_keeps_every_vector_in_scope(self):
        ok, reason = in_scope([MIN_SUBNORMAL_32, ONE_32], MIN_SUBNORMAL_32, FP32, LAX)
        assert ok and reason is None

    def test_subnormal_operand_is_out_of_scope_when_flushed(self):
        ok, reason = in_scope([MIN_SUBNORMAL_32, ONE_32], ONE_32, FP32, FTZ)
        assert not ok
        assert reason == "subnormal_operand"

    def test_subnormal_result_is_out_of_scope_when_flushed(self):
        ok, reason = in_scope([ONE_32, ONE_32], MAX_SUBNORMAL_32, FP32, FTZ)
        assert not ok
        assert reason == "subnormal_result"

    def test_normal_arithmetic_stays_in_scope_when_flushed(self):
        ok, reason = in_scope([ONE_32, ONE_32], 0x40000000, FP32, FTZ)
        assert ok and reason is None

    def test_excluded_vectors_are_counted_not_passed(self):
        c = Comparator(fmt=FP32, profile=FTZ, tier=1)
        c.check(0, [MIN_SUBNORMAL_32, ONE_32], 0x0, ONE_32)
        assert c.checks == 0
        assert c.mismatches == 0
        assert c.excluded == 1
        assert c.exclusions == {"subnormal_operand": 1}

    def test_flushing_does_not_excuse_a_wrong_normal_result(self):
        """The escape hatch must not widen. Normal arithmetic is still bit exact."""
        c = Comparator(fmt=FP32, profile=FTZ, tier=1)
        assert not c.check(0, [ONE_32, ONE_32], 0x40000001, 0x40000000)
        assert c.mismatches == 1
        assert c.excluded == 0

    def test_flushing_does_not_excuse_a_missing_nan(self):
        """Rial returns 0 for 0 x NaN. No subnormal is involved, so it stays a failure."""
        c = Comparator(fmt=FP32, profile=FTZ, tier=1)
        assert not c.check(0, [0x0, 0x7FC00000], 0x0, 0x7FC00000)
        assert c.mismatches == 1
        assert c.excluded == 0


NAN_32 = 0x7FC00000
POS_INF_32 = 0x7F800000
NEG_INF_32 = 0xFF800000


class TestMismatchCategories:
    """12,062 failing vectors are unreadable. The same failures in 25 shapes are a bug report."""

    def test_same_shape_collapses_to_one_category(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        for i in range(500):
            c.check(i, [0x0, NAN_32], 0x0, NAN_32)
        cats, without_exemplar = c.category_report()
        assert c.mismatches == 500
        assert len(cats) == 1
        assert cats[0]["count"] == 500
        assert without_exemplar == 0

    def test_different_shapes_stay_separate(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        c.check(0, [0x0, NAN_32], 0x0, NAN_32)                  # missing NaN
        c.check(1, [ONE_32, ONE_32], 0x40000001, 0x40000000)     # 1 ulp
        cats, _ = c.category_report()
        assert len(cats) == 2
        assert {c["kind"] for c in cats} == {"nan_mismatch", "value"}

    def test_ulp_band_separates_a_rounding_slip_from_a_wild_answer(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        c.check(0, [ONE_32, ONE_32], 0x40000001, 0x40000000)     # 1 ulp
        c.check(1, [ONE_32, ONE_32], 0x50000000, 0x40000000)     # enormous
        cats, _ = c.category_report()
        assert len(cats) == 2
        assert {c["ulp_band"] for c in cats} == {"1", ">1k"}

    def test_operand_order_is_preserved(self):
        """nan x zero and zero x nan are different bugs in a non-commutative implementation."""
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        c.check(0, [NAN_32, 0x0], 0x0, NAN_32)
        c.check(1, [0x0, NAN_32], 0x0, NAN_32)
        cats, _ = c.category_report()
        assert len(cats) == 2

    def test_every_category_carries_one_exemplar(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        for i in range(30):
            c.check(i, [0x0, NAN_32], 0x0, NAN_32)
        cats, _ = c.category_report()
        ex = cats[0]["exemplar"]
        assert ex["index"] == 0
        assert ex["operands"] == ["0x00000000", "0x7fc00000"]
        assert ex["got"] == "0x00000000"
        assert ex["expected"] == "0x7fc00000"

    def test_report_is_bounded_and_reports_what_it_dropped(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        classes = [ONE_32, POS_INF_32, NEG_INF_32, MIN_SUBNORMAL_32, 0x0, NAN_32]
        i = 0
        for a in classes:
            for b in classes:
                c.check(i, [a, b], 0x0, ONE_32)
                i += 1
        cats, without_exemplar = c.category_report(exemplar_limit=5)
        assert without_exemplar > 0
        # Every shape survives; only the exemplar budget is capped.
        assert len(cats) > 5
        assert sum("exemplar" in x for x in cats) == 5
        assert sum(x["count"] for x in cats) == c.mismatches

    def test_categories_are_ordered_by_frequency(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        c.check(0, [ONE_32, ONE_32], 0x40000001, 0x40000000)
        for i in range(1, 10):
            c.check(i, [0x0, NAN_32], 0x0, NAN_32)
        cats, _ = c.category_report()
        assert cats[0]["count"] == 9
        assert cats[1]["count"] == 1

    def test_wrong_infinity_sign_reads_as_a_sign_bug(self):
        c = Comparator(fmt=FP32, profile=LAX, tier=1)
        c.check(0, [ONE_32, NEG_INF_32], POS_INF_32, NEG_INF_32)
        cats, _ = c.category_report()
        assert "wrong sign" in cats[0]["summary"]

    def test_summary_is_a_sentence_not_a_signature(self):
        cat = categorize("nan_mismatch", [0x0, NAN_32], 0x0, NAN_32, FP32, None)
        text = describe(cat)
        assert "zero x nan" in text
        assert "|" not in text


class TestUlpBucket:
    @pytest.mark.parametrize("ulp,band", [
        (None, "na"), (0, "0"), (1, "1"), (0.5, "1"), (3, "2-4"),
        (900, "5-1k"), (100000, ">1k"), (float("inf"), "inf"),
    ])
    def test_bands(self, ulp, band):
        assert ulp_bucket(ulp) == band


class TestLockDrift:
    """The drift check compares port maps, not files, so it works without git."""

    BASE = {
        "module": "FPADD_8_24",
        "ports": [
            {"name": "clock", "dir": "in", "width": 1},
            {"name": "io_in_a", "dir": "in", "width": 32},
            {"name": "io_out", "dir": "out", "width": 32},
        ],
    }

    def _fresh(self, **over):
        import copy
        d = copy.deepcopy(self.BASE)
        d.update(over)
        return d

    def test_identical_maps_do_not_drift(self):
        assert drift(contract(self.BASE), contract(self._fresh())) == []

    def test_reordered_ports_do_not_drift(self):
        """Binding is by name. A reordered port list is not a contract change."""
        shuffled = self._fresh(ports=list(reversed(self.BASE["ports"])))
        assert drift(contract(self.BASE), contract(shuffled)) == []

    def test_tool_version_does_not_drift(self):
        """A Verilator upgrade must not read as 71 broken designs."""
        a = dict(self.BASE, tool="Verilator 5.051")
        b = dict(self.BASE, tool="Verilator 5.060")
        assert drift(contract(a), contract(b)) == []

    def test_renamed_port_shows_as_removed_and_added(self):
        renamed = self._fresh(ports=[
            {"name": "clock", "dir": "in", "width": 1},
            {"name": "io_a", "dir": "in", "width": 32},
            {"name": "io_out", "dir": "out", "width": 32},
        ])
        lines = drift(contract(self.BASE), contract(renamed))
        assert any("removed: io_in_a" in ln for ln in lines)
        assert any("added:   io_a" in ln for ln in lines)

    def test_width_change_is_caught(self):
        wider = self._fresh(ports=[
            {"name": "clock", "dir": "in", "width": 1},
            {"name": "io_in_a", "dir": "in", "width": 64},
            {"name": "io_out", "dir": "out", "width": 32},
        ])
        assert drift(contract(self.BASE), contract(wider)) == ["io_in_a: width 32 -> 64"]

    def test_direction_flip_is_caught(self):
        flipped = self._fresh(ports=[
            {"name": "clock", "dir": "in", "width": 1},
            {"name": "io_in_a", "dir": "out", "width": 32},
            {"name": "io_out", "dir": "out", "width": 32},
        ])
        assert drift(contract(self.BASE), contract(flipped)) == ["io_in_a: direction in -> out"]

    def test_top_module_rename_is_caught(self):
        assert drift(contract(self.BASE), contract(self._fresh(module="FPADD_11_53"))) == [
            "top module FPADD_8_24 -> FPADD_11_53"]


class TestClockResolution:
    """A clock the DUT does not have must not resolve to None and cost the run every edge."""

    class Dut:
        clock = "CLK"
        reset = "RST"

    def test_default_name_resolves(self):
        assert _pin(self.Dut(), {}, "clock", "clock") == "CLK"

    def test_explicit_name_resolves(self):
        assert _pin(self.Dut(), {"clock": "clock"}, "clock", "clock") == "CLK"

    def test_explicit_null_means_no_clock(self):
        assert _pin(self.Dut(), {"clock": None}, "clock", "clock") is None

    def test_undeclared_and_absent_stays_lenient(self):
        """Omitting the field claims nothing, so a clockless DUT is fine."""
        assert _pin(object(), {}, "clock", "clock") is None

    def test_declared_but_absent_raises(self):
        with pytest.raises(RuntimeError, match="clk_i"):
            _pin(self.Dut(), {"clock": "clk_i"}, "clock", "clock")

    def test_reset_gets_the_same_treatment(self):
        with pytest.raises(RuntimeError, match="rst_n"):
            _pin(self.Dut(), {"reset": "rst_n"}, "reset", "reset")
