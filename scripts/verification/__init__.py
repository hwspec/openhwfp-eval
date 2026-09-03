"""Descriptor-driven FP verification.

Tier 1 compares against SoftFloat via testfloat_gen and is bit exact.
Tier 2 compares against MPFR within a declared ULP budget.
A capability profile decides how much of the DUT each run is allowed to check.
"""
