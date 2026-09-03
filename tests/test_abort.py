"""A DUT $stop must become an 'aborted' record, and the summarizer must tolerate it."""

import json
import os
import tempfile

from scripts.verification.record import write_aborted
from scripts.verification.summarize import rollup


def _job(result_path):
    return {
        "design": {"design_id": "rial/RialSinFP32", "library": "rial", "operator": "sin",
                   "format": {"name": "fp32"}, "descriptor_path": "d.yaml",
                   "conformance_level": "minimal", "tier": 2},
        "rounding": "rne", "tininess": "after",
        "vectors": {"function": "sin"}, "result_path": result_path,
    }


def test_write_aborted_produces_a_row():
    path = os.path.join(tempfile.mkdtemp(), "r.json")
    rec = write_aborted(_job(path), "Assertion failed at sincos.scala:237")
    assert rec["status"] == "aborted"
    assert "sincos.scala:237" in rec["abort_reason"]
    assert rec["checks_performed"] == 0
    assert json.load(open(path))["status"] == "aborted"


def test_summarizer_tolerates_aborted_records():
    path = os.path.join(tempfile.mkdtemp(), "r.json")
    rec = write_aborted(_job(path), "boom")
    rows = rollup([rec])                       # must not KeyError on missing metrics
    assert rows[0]["status"] == "aborted"
    assert rows[0]["aborted"] == 1
