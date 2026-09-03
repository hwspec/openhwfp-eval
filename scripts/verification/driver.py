"""cocotb testbench driven entirely by a manifest entry.

Port names come from the descriptor, so this module never mentions a library. Adding a library
means adding a YAML, not editing this file. Adding a handshake nobody has seen before means
adding one protocol branch.
"""

from __future__ import annotations

import json
import os
from collections import deque

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import ClockCycles, RisingEdge, Timer

from .capabilities import ROUNDING, TININESS, Profile
from .compare import Comparator, compare_value
from .formats import FpFormat
from .record import write_result
from .stimulus.testfloat import VectorSet

JOB_ENV = "OPENHWFP_JOB"


def _load_job():
    path = os.environ.get(JOB_ENV)
    if not path:
        raise RuntimeError(f"{JOB_ENV} is not set; launch through scripts.verification.run")
    with open(path) as fh:
        return json.load(fh)


def _bind(dut, job):
    """Resolve every role to a handle up front. A missing signal fails here, not mid-run."""
    handles = {}
    missing = []
    for role, spec in job["design"]["ports"].items():
        rtl = spec["rtl"]
        try:
            handles[role] = getattr(dut, rtl)
        except AttributeError:
            missing.append(f"{role} -> {rtl}")
    if missing:
        raise RuntimeError(
            "ports declared in the descriptor are absent from the elaborated DUT: "
            + ", ".join(missing)
            + ". Re-run scripts/scaffold.py and scripts/build_manifest.py.")
    return handles


def _get(handle) -> int:
    v = handle.value
    try:
        return int(v)
    except (ValueError, TypeError):
        return int(v.to_unsigned())


def _pin(dut, sim, field, default):
    """Resolve a clock or reset. Explicit null means the design has none.

    A name the DUT does not carry is an error, never a silent skip. Missing the clock costs you
    every edge in the run and looks exactly like broken hardware.
    """
    name = sim.get(field, default)
    if name is None:
        return None
    handle = getattr(dut, name, None)
    if handle is None and field in sim:
        raise RuntimeError(
            f"sim.{field} names '{name}', which the DUT does not have. "
            f"Fix the descriptor or set it to null.")
    return handle


async def _reset(dut, job, handles):
    sim = job["design"]["sim"]
    clk = _pin(dut, sim, "clock", "clock")
    if clk is not None:
        cocotb.start_soon(Clock(clk, 10, unit="ns").start())

    rst = _pin(dut, sim, "reset", "reset")
    active = 1 if sim.get("reset_active", "high") == "high" else 0
    if rst is not None:
        rst.value = active
        if clk is not None:
            await ClockCycles(clk, sim.get("reset_cycles", 10))
        else:
            await Timer(10, unit="ns")
        rst.value = 1 - active
        if clk is not None:
            await ClockCycles(clk, 2)
    return clk


def _apply_constants(job, handles):
    for role, spec in job["design"]["ports"].items():
        if "constant" in spec and role in handles:
            handles[role].value = int(spec["constant"])


def _apply_controls(job, handles):
    """Drive rounding mode and tininess when the DUT exposes them."""
    if "rounding_mode" in handles:
        handles["rounding_mode"].value = ROUNDING[job["rounding"]][1]
    if "tininess" in handles:
        handles["tininess"].value = TININESS[job["tininess"]][1]


def _drive_operands(handles, operands):
    for role, value in zip(("a", "b", "c"), operands):
        if role in handles:
            handles[role].value = value


def _read_result(handles):
    got = _get(handles["result"])
    flags = _get(handles["flags"]) if "flags" in handles else None
    return got, flags


@cocotb.test()
async def run_vectors(dut):
    job = _load_job()
    design = job["design"]
    fmt = FpFormat.from_manifest(design["format"])
    profile = Profile.from_manifest(design["profile"])
    cmp = Comparator(fmt=fmt, profile=profile, tier=design["tier"])

    vectors = VectorSet(**{**job["vectors"], "path": job["vectors"]["path"]})
    handles = _bind(dut, job)
    clk = await _reset(dut, job, handles)
    _apply_constants(job, handles)
    _apply_controls(job, handles)

    protocol = design["sim"]["protocol"]
    limit = job.get("max_vectors")

    dut._log.info(
        f"{design['design_id']} {protocol} {job['rounding']}/{job['tininess']} "
        f"tier{design['tier']} flags={profile.flag_check} vectors={vectors.count}")

    if protocol == "combinational":
        await _run_combinational(dut, handles, vectors, cmp, limit)
    elif protocol == "fixed_latency":
        await _run_fixed_latency(dut, clk, handles, vectors, cmp, design["sim"]["latency"], limit)
    elif protocol == "valid_poll":
        await _run_valid_poll(dut, clk, handles, vectors, cmp, limit)
    else:
        raise RuntimeError(f"no driver for protocol '{protocol}'")

    await _canary(dut, handles, cmp, fmt, protocol, clk, design)

    write_result(job, cmp, vectors)

    if cmp.mismatches:
        first = "\n  ".join(m.render(fmt) for m in cmp.recorded[:10])
        raise AssertionError(
            f"{cmp.mismatches} mismatches in {cmp.checks} checks\n  {first}")
    if cmp.checks == 0:
        raise AssertionError("ran 0 checks; the vector file or the driver is wrong")


async def _run_combinational(dut, handles, vectors, cmp, limit):
    for i, (operands, expected, expected_flags) in enumerate(vectors.rows()):
        if limit and i >= limit:
            break
        _drive_operands(handles, operands)
        await Timer(1, unit="ns")
        got, flags = _read_result(handles)
        cmp.check(i, operands, got, expected, flags, expected_flags)


async def _run_fixed_latency(dut, clk, handles, vectors, cmp, latency, limit):
    """Stream one vector per cycle; results emerge `latency` edges behind.

    A vector pushed at iteration j has seen len(inflight) edges by the time we sample, so the
    head is ready once that count reaches latency. Off by one here silently compares vector i
    against vector i+1's expectation, which looks like a catastrophic DUT failure.

    out_valid is high from reset on these modules and never falls, so latency is the contract
    and the valid bit is decoration.
    """
    inflight = deque()
    if "valid_in" in handles:
        handles["valid_in"].value = 1

    for i, (operands, expected, expected_flags) in enumerate(vectors.rows()):
        if limit and i >= limit:
            break
        _drive_operands(handles, operands)
        inflight.append((i, operands, expected, expected_flags))
        await RisingEdge(clk)
        await Timer(1, unit="ns")
        if len(inflight) >= latency:
            j, ops, exp, exp_f = inflight.popleft()
            got, flags = _read_result(handles)
            cmp.check(j, ops, got, exp, flags, exp_f)

    if "valid_in" in handles:
        handles["valid_in"].value = 0
    while inflight:
        await RisingEdge(clk)
        await Timer(1, unit="ns")
        j, ops, exp, exp_f = inflight.popleft()
        got, flags = _read_result(handles)
        cmp.check(j, ops, got, exp, flags, exp_f)


async def _run_valid_poll(dut, clk, handles, vectors, cmp, limit, timeout_cycles=4000):
    valid_in = handles.get("valid_in")
    valid_out = handles.get("valid_out")
    ready_in = handles.get("ready_in")

    for i, (operands, expected, expected_flags) in enumerate(vectors.rows()):
        if limit and i >= limit:
            break

        if ready_in is not None:
            waited = 0
            while not _get(ready_in):
                await RisingEdge(clk)
                waited += 1
                if waited > timeout_cycles:
                    raise AssertionError(f"vector {i}: DUT never asserted ready_in")

        _drive_operands(handles, operands)
        if valid_in is not None:
            valid_in.value = 1
        await RisingEdge(clk)
        if valid_in is not None:
            valid_in.value = 0

        waited = 0
        while valid_out is not None and not _get(valid_out):
            await RisingEdge(clk)
            waited += 1
            if waited > timeout_cycles:
                raise AssertionError(f"vector {i}: DUT never asserted valid_out")
        await Timer(1, unit="ns")
        got, flags = _read_result(handles)
        cmp.check(i, operands, got, expected, flags, expected_flags)
        await RisingEdge(clk)


async def _canary(dut, handles, cmp, fmt, protocol, clk, design):
    """Feed one deliberately wrong expectation and confirm the comparator notices.

    Static checks cannot catch a port that exists but is stuck. This can. If the canary passes,
    the run is not measuring anything and the result is void.

    The wrong value is always a normal number so a flush-to-zero profile cannot excuse it, and
    the check goes straight to compare_value so the scope gate cannot swallow it either.
    """
    operands = [0x0] * max(1, sum(1 for r in ("a", "b", "c") if r in handles))
    _drive_operands(handles, operands)
    if protocol == "combinational":
        await Timer(1, unit="ns")
    else:
        await ClockCycles(clk, design["sim"].get("latency", 1) + 2)
        await Timer(1, unit="ns")
    got, _ = _read_result(handles)

    one = fmt.bias << fmt.stored_mantissa_bits
    two = (fmt.bias + 1) << fmt.stored_mantissa_bits
    wrong = two if got == one else one

    verdict = compare_value(got, wrong, fmt, cmp.profile, cmp.tier)
    if verdict.ok:
        raise AssertionError(
            f"canary vector was not flagged: DUT gave {fmt.hex(got)}, comparator accepted "
            f"{fmt.hex(wrong)}. The comparator is not connected to the DUT.")
    cmp.canary_ok = True
