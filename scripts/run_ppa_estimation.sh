#!/bin/bash
# Yosys PPA (area) workflow
# Step 1: Generate Verilog for all test modules
# Step 2: Run Yosys for area estimation + HTML report

set -e  # Exit on error

echo "=========================================="
echo "Yosys PPA Workflow for All Test Modules"
echo "=========================================="

# Step 0: Clean generated folder
echo ""
echo "Step 0: Cleaning generated folder..."
if [ -d "generated" ]; then
    rm -rf generated/*
    echo "  ✓ Cleaned generated/ folder"
else
    echo "  ℹ generated/ folder doesn't exist yet (will be created)"
fi

# Step 1: Generate Verilog
echo ""
echo "Step 1: Generating Verilog from Chisel modules..."
echo "This will generate .sv files for OpenFloat, HardFloat, and Rial modules"
sbt "test:runMain Generate.GenerateAllTestModules"

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
