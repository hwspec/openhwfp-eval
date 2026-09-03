#!/bin/bash
# Yosys PPA (area) workflow
# Step 1: Generate Verilog for all test modules
# Step 2: Run Yosys for area estimation + HTML report

set -e  # Exit on error

echo "=========================================="
echo "Yosys PPA Workflow for All Test Modules"
echo "=========================================="

PY_BIN="${PY_BIN:-python3}"
[ -x .venv/bin/python ] && PY_BIN=.venv/bin/python

# Single-design mode: RTL and manifest are already built; synth just this one design
# into a scoped XML and update/insert its row.
DESIGN="${DESIGN:-}"
if [ -n "$DESIGN" ]; then
    lib="${DESIGN%%/*}"
    stem="${DESIGN##*/}"
    sv="generated/${lib}/${stem}.sv"
    xml="generated/cell_count_${lib}_${stem}.xml"
    if [ ! -f "$sv" ]; then
        echo "ERROR: $sv not found. Run 'make build' (or 'make rtl') first."
        exit 1
    fi
    echo ""
    echo "Single-design ppa: $DESIGN"
    PPA_XML_OUT="$xml" python3 scripts/estimate.py "$sv"
    "$PY_BIN" scripts/export_flow_instances.py --design "$DESIGN" --xml "$xml"
    exit $?
fi

# Step 0: Clear only the Yosys artifacts: the RTL, manifest, plan and generator constants under
# generated/ are what the verification flow depends on and are regenerated below anyway; wiping all
# of generated/ would destroy them mid-run.
echo ""
echo "Step 0: Clearing previous Yosys artifacts..."
rm -f generated/cell_count_report.xml generated/cell_count_report.html generated/ppa_report.html
echo "  ✓ Cleared Yosys reports"

# Step 1: Generate Verilog
echo ""
echo "Step 0: Building the descriptor manifest..."
PY_BIN="${PY_BIN:-python3}"
[ -x .venv/bin/python ] && PY_BIN=.venv/bin/python
"$PY_BIN" scripts/build_manifest.py

if [ $? -ne 0 ]; then
    echo "Error: descriptor contract failed; fix the descriptors before generating"
    exit 1
fi

echo ""
echo "Step 1: Generating Verilog from descriptors..."
echo "Every design named in generated/elaboration_plan.json"
sbt "runMain Generate.GenerateAllTestModules"

if [ $? -ne 0 ]; then
    echo "Error: Verilog generation failed"
    exit 1
fi

# Step 2: Run Yosys for quick PPA estimates
echo ""
echo "Step 2: Running Yosys for PPA estimation..."
echo "Analyzing all generated .sv files..."

# Clear prior Yosys log so error report reflects this run only
rm -f yosys_output.log

# Run estimate.py on all generated files (from root directory)
python3 scripts/estimate.py "generated/**/*.sv"
yosys_exit_code=$?

if [ $yosys_exit_code -ne 0 ]; then
    echo "Warning: Yosys estimation failed or Yosys not available"
    echo "Checking for existing XML report..."
    
    if [ -f "generated/cell_count_report.xml" ]; then
        echo "  ✓ Found existing XML report, will generate HTML from it"
    else
        echo "  ✗ No existing XML report found"
        echo "  Please install Yosys to generate PPA estimates"
        echo "  Installation: https://github.com/YosysHQ/yosys/releases"
        echo "  Or use conda: conda install -c litex-hub yosys"
    fi
fi

# Step 2b: Generate HTML report (if XML exists)
if [ -f "generated/cell_count_report.xml" ]; then
    echo ""
    echo "Step 2b: Generating HTML report..."
    python3 scripts/xml_to_html.py generated/cell_count_report.xml generated/ppa_report.html
    
    if [ $? -eq 0 ]; then
        echo "  ✓ HTML report generated: generated/ppa_report.html"
    else
        echo "  ✗ HTML report generation failed"
    fi
else
    echo ""
    echo "Step 2b: Skipping HTML report (no XML data available)"
fi

echo ""
echo "=========================================="
echo "Workflow complete!"
echo "=========================================="
echo "Generated Verilog: generated/"
echo "  - OpenFloat: generated/openfloat/"
echo "  - HardFloat: generated/hardfloat/"
echo "  - Rial: generated/rial/"
echo ""
echo "Yosys results: generated/cell_count_report.xml"
echo "HTML report: generated/ppa_report.html"
echo "=========================================="

# Step 3: flow-instance records (Table 2 / dataset schema)
echo ""
echo "Step 3: Exporting flow-instance records..."
if python3 scripts/export_flow_instances.py; then
    echo "  ✓ dataset/flow_instances.jsonl"
    echo "  Archive with: bash scripts/archive_phase1.sh"
else
    echo "  ✗ export failed (Yosys XML still in generated/)"
fi
