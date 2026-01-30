#!/usr/bin/env python3
"""
Run Yosys analysis on SystemVerilog files to estimate PPA (Power, Performance, Area).
Preprocesses files to remove SystemVerilog features not supported by older Yosys versions.
"""

import subprocess
import re
import glob
import sys
import xml.etree.ElementTree as ET
from datetime import datetime
import multiprocessing
from concurrent.futures import ProcessPoolExecutor, as_completed
from functools import lru_cache
import os
from typing import Tuple, List, Dict
import logging
import concurrent.futures
import time
import threading
import math

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# Constants
AREA_PER_CELL = 100  # nm² for 7nm technology
CACHE_SIZE = 128  # Number of results to cache
MAX_WORKERS = multiprocessing.cpu_count()  # Use all available CPU cores
TIMEOUT = 600  # Increased timeout to 10 minutes

@lru_cache(maxsize=CACHE_SIZE)
def prettify_name(filename: str) -> str:
    # Remove file extension
    name = os.path.splitext(filename)[0]
    
    # Handle Flopoco naming pattern - preserve FLOPCO_FP and everything after it
    if name.startswith('FLOPCO_FP'):
        return name  # Return the name as is, preserving FLOPCO_FP and everything after
    
    # Map exponent/mantissa to FP type
    fp_map = {
        ('5', '10'): 'FP16',
        ('8', '23'): 'FP32',
        ('8', '24'): 'FP32',  # Handle HardFloat format
        ('11', '52'): 'FP64',
        ('11', '53'): 'FP64'  # Handle new HardFloat FP64 format
    }

    # Handle CFG naming pattern (e.g., CFG_FP_add_16_1.sv -> CFGadd_FP16)
    cfg_match = re.match(r'CFG_FP_(\w+)_(\d+)_(\d+)', name)
    if cfg_match:
        op, bits, _ = cfg_match.groups()
        if bits == '16':
            return f"CFG{op}_FP16"
        elif bits == '32':
            return f"CFG{op}_FP32"
        elif bits == '64':
            return f"CFG{op}_FP64"

    # Handle HardFloat naming pattern (e.g., FPADD_8_24 -> HardFloatADD_FP32)
    hardfloat_match = re.match(r'FP(\w+)_(\d+)_(\d+)', name)
    if hardfloat_match:
        op, e, m = hardfloat_match.groups()
        fp = fp_map.get((e, m), f"e{e}_m{m}")
        return f"HardFloat{op}_{fp}"

    # Handle OpenFloat naming pattern (e.g., FP_add_32_1 -> OpenFloatAdd_FP32)
    openfloat_match = re.match(r'FP_(\w+)_(\d+)_(\d+)', name)
    if openfloat_match:
        op, bits, _ = openfloat_match.groups()
        if bits == '16':
            return f"OpenFloat{op}_FP16"
        elif bits == '32':
            return f"OpenFloat{op}_FP32"
        elif bits == '64':
            return f"OpenFloat{op}_FP64"
    
    # Handle OpenFloat divider/sqrt pattern (e.g., FP_divider_32_15_15 -> OpenFloatDiv_FP32)
    openfloat_div_match = re.match(r'FP_(\w+)_(\d+)_(\d+)_(\d+)', name)
    if openfloat_div_match:
        op, bits, _, _ = openfloat_div_match.groups()
        if bits == '16':
            return f"OpenFloat{op}_FP16"
        elif bits == '32':
            return f"OpenFloat{op}_FP32"
        elif bits == '64':
            return f"OpenFloat{op}_FP64"

    # Handle Rial naming pattern (e.g., RialAddFP16 -> RialAdd_FP16)
    rial_match = re.match(r'Rial(\w+)(FP\d+)', name)
    if rial_match:
        op, fp = rial_match.groups()
        return f"Rial{op}_{fp}"

    # Custom replacements for composite names
    name = name.replace('ReciprocalATan2Phase1ATan2Phase2', 'Atan2')
    name = name.replace('SqrtACosPhase1ACosPhase2', 'Acos')
    
    # Try to match the main pattern
    m = re.match(r'(Rial\w+)(_e(\d+)_m(\d+)_s\d+)', name)
    if m:
        base, _, e, m_ = m.groups()
        fp = fp_map.get((e, m_), f"e{e}_m{m_}")
        return f"{base}_{fp}"
    
    # Try to match composite/phase pattern
    m = re.match(r'(Rial\w+Phase1\w+Phase2\w+)(_e(\d+)_m(\d+)_s\d+)', name)
    if m:
        base, _, e, m_ = m.groups()
        fp = fp_map.get((e, m_), f"e{e}_m{m_}")
        return f"{base}_{fp}"
    
    # Fallback: just return the name without extension
    return name

def _hoist_logic_from_always(content: str) -> str:
    """Hoist 'logic x = e;' inside always blocks to module-level 'wire x = e;'."""
    lines = content.split('\n')
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        # Start of always block: "always @(...) begin"
        m = re.match(r'^(\s*)always\s+@\s*\([^)]+\)\s+begin\b', line)
        if m:
            indent = m.group(1)
            always_start = len(out)
            out.append(line)
            i += 1
            depth = 1
            hoisted = []
            while i < len(lines) and depth > 0:
                cur = lines[i]
                rest = cur.split('//')[0]
                if re.search(r'\bbegin\b', rest):
                    depth += 1
                if re.search(r'\bend\b', rest) and not re.search(r'\bend(endmodule|function|case|task|primitive|generate)\b', rest):
                    depth -= 1
                    if depth == 0:
                        # End of this always: insert hoisted wires before the block (source order)
                        for idx, w in enumerate(hoisted):
                            out.insert(always_start + idx, w)
                        out.append(cur)
                        i += 1
                        continue
                    # nested end
                    out.append(cur)
                    i += 1
                    continue
                # Inside always: look for "logic [N:M] name =" or "logic name ="
                logic_m = re.match(r'^(\s*)logic\s+(\[[^\]]+\]\s+)?(\w+)\s*=\s*(.*)$', cur)
                if logic_m and depth >= 1:
                    decl_indent, typ, name, rhs = logic_m.group(1), logic_m.group(2) or '', logic_m.group(3), logic_m.group(4)
                    stmt = [cur]
                    j = i + 1
                    while ';' not in rhs and j < len(lines):
                        stmt.append(lines[j])
                        rhs += '\n' + lines[j]
                        j += 1
                    full = '\n'.join(stmt)
                    # Extract RHS: from first '=' to last ';'
                    eq = full.find('=')
                    semi = full.rfind(';')
                    if eq != -1 and semi != -1:
                        rhs = full[eq + 1:semi].strip()
                        # Collapse newlines so Yosys gets a single-line wire
                        rhs = ' '.join(rhs.split())
                        wire = f"{indent}wire {typ}{name} = {rhs};"
                        hoisted.append(wire)
                    i = j
                    continue
                out.append(cur)
                i += 1
            continue
        out.append(line)
        i += 1
    return '\n'.join(out)

def preprocess_sv_file(filename: str) -> str:
    """Preprocess SystemVerilog file to remove features Yosys doesn't support."""
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Remove 'automatic' keyword (Yosys doesn't support it)
        # Process line by line to handle indentation properly
        lines = content.split('\n')
        cleaned_lines = []
        for line in lines:
            # Remove "automatic" keyword - handle various patterns:
            # "      automatic logic [8:0] var ="
            # "automatic logic var"
            # etc.
            # First try to match "automatic" followed by a type keyword
            cleaned_line = re.sub(r'\bautomatic\s+(logic|reg|wire|integer|real|time|int|bit|byte|shortint|longint|shortreal|string|chandle|event|struct|union|enum|class|interface|modport|package|program|task|function)\s+', r'\1 ', line)
            # Then remove any remaining "automatic " (catch-all)
            cleaned_line = re.sub(r'\bautomatic\s+', '', cleaned_line)
            cleaned_lines.append(cleaned_line)
        content = '\n'.join(cleaned_lines)
        
        # Remove include statements for verification files (handle leading whitespace)
        # Handle formats like:
        #   `include "layers-XXX-Verification.sv"
        #     `include "layers-XXX-Verification.sv" // comment (with leading spaces/tabs)
        #   `include "layers-XXX-Verification.sv"    /* comment */
        lines = content.split('\n')
        cleaned_lines = []
        for line in lines:
            # Check if line contains an include for verification/layers
            # Handle leading whitespace before backtick
            if '`include' in line:
                # Check if it's a verification/layers include (case insensitive)
                line_lower = line.lower()
                if 'layers-' in line_lower or 'verification' in line_lower:
                    # Skip this entire line
                    continue
            cleaned_lines.append(line)
        content = '\n'.join(cleaned_lines)
        
        # Final regex pass to catch any remaining includes (handles edge cases)
        # Match includes with optional leading whitespace
        content = re.sub(r'[^\S\n]*`include\s+["\'][^"\']*layers-[^"\']*["\'][^\n]*\n?', '', content)
        content = re.sub(r'[^\S\n]*`include\s+["\'][^"\']*verification[^"\']*["\'][^\n]*\n?', '', content, flags=re.IGNORECASE)
        
        # Remove verification-related sections (everything after "FILE" markers)
        # Split by FILE markers and keep only the first module
        lines = content.split('\n')
        cleaned_lines = []
        skip_verification = False
        for line in lines:
            if 'FILE "verification/' in line or 'FILE "layers-' in line:
                skip_verification = True
            if not skip_verification:
                cleaned_lines.append(line)
            elif line.strip() == '' and skip_verification:
                # Stop skipping after empty line (end of verification section)
                skip_verification = False
        
        content = '\n'.join(cleaned_lines)
        
        # Remove ifndef/define/endif blocks for verification
        content = re.sub(r'`ifndef\s+layers_[^\n]+\n\s*`define[^\n]+\n\s*`endif[^\n]*\n', '', content)
        
        # Remove empty ifndef/endif blocks
        content = re.sub(r'`ifndef\s+\w+\s*\n\s*`define\s+\w+\s*\n\s*`endif[^\n]*\n', '', content)
        
        # Remove ENABLE_INITIAL_REG_ / initial blocks (Yosys "invalid nesting" with
        # initial + for(logic...) + automatic logic). Not needed for area-only synthesis.
        lines = content.split('\n')
        cleaned = []
        skip_depth = 0
        for line in lines:
            if 'ENABLE_INITIAL_REG_' in line:
                if re.search(r'`ifdef\s+ENABLE_INITIAL_REG_', line):
                    skip_depth += 1
                    continue
                if re.search(r'`ifndef\s+ENABLE_INITIAL_REG_', line):
                    skip_depth = 1
                    continue
                if re.search(r'`endif.*ENABLE_INITIAL_REG_', line):
                    skip_depth -= 1
                    continue
            if skip_depth > 0:
                if re.search(r'`(?:ifdef|ifndef)\b', line):
                    skip_depth += 1
                elif re.search(r'`endif\b', line):
                    skip_depth -= 1
                continue
            cleaned.append(line)
        content = '\n'.join(cleaned)
        
        # Hoist "logic x = e;" from inside always blocks to module-level "wire x = e;"
        # (Yosys "invalid nesting" otherwise.)
        content = _hoist_logic_from_always(content)
        
        # Fix OP_CAST / unpacked array literal: Yosys errors on "'{ ... }" (SV array
        # assignment pattern). Rewrite as concatenation "{ ... }".
        content = content.replace("'{", "{")
        
        return content
    except Exception as e:
        logger.error(f"Error preprocessing {filename}: {e}")
        import traceback
        logger.debug(f"Preprocessing traceback: {traceback.format_exc()}")
        return None

def get_top_module(filename: str) -> str:
    """Get the top module name for a SystemVerilog file."""
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            content = f.read()
        base = os.path.splitext(os.path.basename(filename))[0]
        module_pattern = re.compile(r'\bmodule\s+(\w+)\s*(?:\(|#)', re.MULTILINE)
        candidates = []
        for m in module_pattern.finditer(content):
            name = m.group(1)
            if any(skip in name.lower() for skip in ['verification', 'assert', 'assume', 'cover', 'layer']):
                continue
            candidates.append(name)
        if not candidates:
            return base
        # Prefer module matching filename (e.g. FP_mult_32_1.sv -> FP_mult_32_1)
        for c in candidates:
            if c == base:
                return c
        # Else use last module (Chisel often emits submodules first, top last)
        return candidates[-1]
    except Exception as e:
        logger.debug(f"Error reading {filename} for module name: {e}")
        return os.path.splitext(os.path.basename(filename))[0]

def get_top_entity(filename: str) -> str:
    """Get the top entity name for a VHDL file."""
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            content = f.read()
            # Find all entity declarations
            last_entity_word = None
            lines = content.splitlines()
            
            for line in lines:
                # Case-insensitive search for whole word 'entity'
                if re.search(r'\bentity\b', line, re.IGNORECASE):
                    # Find the last 'entity' in this line
                    match = re.finditer(r'\bentity\b', line, re.IGNORECASE)
                    for m in match:
                        start_idx = m.end()
                        # Extract the rest of the line after 'entity'
                        rest_of_line = line[start_idx:].strip()
                        # Find the next word (sequence of non-whitespace characters)
                        next_word_match = re.match(r'\s*(\w+)', rest_of_line)
                        if next_word_match:
                            last_entity_word = next_word_match.group(1)
            
            if last_entity_word:
                logger.info(f"Found top entity {last_entity_word} in {filename}")
                return last_entity_word
            else:
                logger.error(f"No entity declaration found in {filename}")
    except Exception as e:
        logger.error(f"Error reading {filename}: {e}")
    return None

def setup_logging():
    """Configure logging with a custom format."""
    logger = logging.getLogger()
    logger.setLevel(logging.INFO)
    
    # Clear any existing handlers to prevent duplicates
    logger.handlers = []
    
    # Create console handler with custom format
    console_handler = logging.StreamHandler()
    formatter = logging.Formatter('%(asctime)s - %(levelname)s - %(message)s')
    console_handler.setFormatter(formatter)
    
    # Don't filter - show all messages for debugging
    logger.addHandler(console_handler)
    
    return logger

def sum_cell_counts(text: str) -> int:
    """Parse cell counts from Yosys output."""
    # Split the text into lines
    lines = text.strip().split('\n')
    
    # Find the last occurrence of "Printing statistics."
    last_stats_index = -1
    for i, line in enumerate(lines):
        if "Printing statistics." in line:
            last_stats_index = i
    
    # If "Printing statistics." not found, return 0
    if last_stats_index == -1:
        return 0
    
    # Initialize sum of cell counts
    total_cells = 0
    
    # Process lines after the last "Printing statistics."
    for line in lines[last_stats_index:]:
        # Match "Number of cells: <number>"
        match = re.match(r'\s*Number of cells:\s*(\d+)', line)
        if match:
            total_cells += int(match.group(1))
    
    return total_cells

def yosys(fn: str) -> Tuple[int, int]:
    """Run Yosys analysis on a single file."""
    temp_file = f"tmp_{os.getpid()}.ys"
    preprocessed_file = None
    try:
        # Determine file type and appropriate Yosys commands
        if fn.endswith('.vhdl'):
            # For VHDL files (Flopoco)
            top = get_top_entity(fn)
            if not top:
                logger.error(f"Could not find top entity in {fn}")
                return (0, 0)
            yosyscmd = f"ghdl --std=08 -fsynopsys {fn} -e {top}; synth; stat"
            logger.info(f"Processing VHDL file: {fn} with top entity: {top}")
        else:
            # For SystemVerilog files - try preprocessing first, but fallback to direct read
            # Get actual module name from file (not filename-based, since they don't match)
            top = get_top_module(fn)
            
            # Try preprocessing to remove problematic SystemVerilog features
            preprocessed_content = preprocess_sv_file(fn)
            if preprocessed_content is None:
                logger.warning(f"Preprocessing failed for {fn}, using original file (may have Yosys compatibility issues)")
            if preprocessed_content is not None:
                # Write preprocessed content to temporary file
                preprocessed_file = f"tmp_{os.getpid()}_{os.path.basename(fn)}"
                with open(preprocessed_file, 'w', encoding='utf-8') as f:
                    f.write(preprocessed_content)
                # Use preprocessed file
                abs_path = os.path.abspath(preprocessed_file)
                yosyscmd = f"""read_verilog -sv {abs_path}
hierarchy -top {top}
proc
techmap -map +/techmap.v
opt_clean
stat
"""
            else:
                # Fallback: use original file directly (like old script)
                abs_path = os.path.abspath(fn)
                yosyscmd = f"""read -sv {abs_path}
hierarchy -top {top}
proc
techmap -map +/techmap.v
opt_clean
stat
"""

        with open(temp_file, 'w') as f:
            f.write(yosyscmd)
        
        try:
            # Run Yosys with appropriate command based on file type
            # Capture both stdout and stderr to see what's going wrong
            if fn.endswith('.vhdl'):
                result = subprocess.run(['yosys', '-m', 'ghdl', '-p', yosyscmd],
                                       encoding='utf8',
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE,
                                       timeout=TIMEOUT)
                p = result.stdout
                stderr_output = result.stderr
            else:
                result = subprocess.run(['yosys', temp_file],
                                       encoding='utf8',
                                       stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE,
                                       timeout=TIMEOUT)
                p = result.stdout
                stderr_output = result.stderr
            
            # Log Yosys output to file (relative to script location)
            script_dir = os.path.dirname(os.path.abspath(__file__))
            log_file = os.path.join(script_dir, '..', 'yosys_output.log')
            with open(log_file, 'a', encoding='utf-8') as f:
                f.write(f"\n=== Processing {fn} ===\n")
                f.write(f"Top module: {top}\n")
                if preprocessed_file:
                    f.write(f"Using preprocessed file: {preprocessed_file}\n")
                f.write(f"Yosys commands:\n{yosyscmd}\n")
                f.write("Yosys stdout:\n")
                f.write(p)
                f.write("\nYosys stderr:\n")
                f.write(stderr_output)
                f.write("\n")
            
            # Check if Yosys failed - but don't exit immediately, try to parse anyway
            # (old script ignored errors and tried to parse)
            if result.returncode != 0:
                # Log error but continue - might still have partial results
                if stderr_output:
                    error_lines = [line.strip() for line in stderr_output.split('\n') if 'ERROR' in line]
                    if error_lines:
                        logger.debug(f"Yosys error for {fn}: {error_lines[0][:150]}")
                # Don't return immediately - try to parse what we got
                
        except subprocess.TimeoutExpired:
            logger.error(f"Timeout processing {fn}")
            return (0, 0)
        except FileNotFoundError:
            logger.error(f"Yosys not found. Please install Yosys and ensure it's in your PATH.")
            return (0, 0)
        except Exception as e:
            logger.error(f"Unexpected error running Yosys for {fn}: {str(e)}")
            return (0, 0)
        
        # Parse Yosys output
        wire_bits = 0
        cells = 0
        if fn.endswith('.vhdl'):
            cells = sum_cell_counts(p)
        else:
            # Yosys 0.61 uses different output format - look for both patterns
            for line in p.split('\n'):
                # Try old format "Number of wire bits: 397"
                if "Number of wire bits:" in line:
                    try:
                        wire_bits = int(re.search(r'\d+', line).group())
                    except (AttributeError, ValueError):
                        pass
                # Try new format "      397 wire bits" (Yosys 0.61 - with leading spaces)
                elif re.search(r'\d+\s+wire bits', line) and not "Number of" in line:
                    try:
                        match = re.search(r'(\d+)\s+wire bits', line)
                        if match:
                            wire_bits = int(match.group(1))
                    except (AttributeError, ValueError):
                        pass
                # Try old format "Number of cells: 52"
                elif "Number of cells:" in line:
                    try:
                        cells = int(re.search(r'\d+', line).group())
                    except (AttributeError, ValueError):
                        pass
                # Try new format "       52 cells" (Yosys 0.61 - with leading spaces, standalone)
                elif re.search(r'^\s+\d+\s+cells\s*$', line):
                    try:
                        match = re.search(r'(\d+)\s+cells', line)
                        if match:
                            cells = int(match.group(1))
                    except (AttributeError, ValueError):
                        pass
        
        if fn.endswith('.vhdl'):
            logger.info(f"VHDL file {fn} processed: {cells} cells")
        
        return (wire_bits, cells)
    except Exception as e:
        logger.error(f"Unexpected error processing {fn}: {str(e)}")
        return (0, 0)
    finally:
        if os.path.exists(temp_file):
            try:
                os.remove(temp_file)
            except OSError as e:
                logger.warning(f"Could not remove temporary file {temp_file}: {e}")
        if preprocessed_file and os.path.exists(preprocessed_file):
            try:
                os.remove(preprocessed_file)
            except OSError as e:
                logger.warning(f"Could not remove preprocessed file {preprocessed_file}: {e}")

def estimate_area_nm2(ncells: int, a: int = AREA_PER_CELL) -> float:
    """Estimate total area in nm² for 7nm technology."""
    return ncells * a

def create_xml_report(results: List[Tuple[str, Tuple[int, int]]]) -> None:
    """Create XML report from analysis results."""
    try:
        # Create the root element
        root = ET.Element("CellCountReport")
        
        # Add timestamp
        timestamp = ET.SubElement(root, "Timestamp")
        timestamp.text = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        
        # Add summary
        summary = ET.SubElement(root, "Summary")
        total_cells = sum(cells[1] for _, cells in results)
        ET.SubElement(summary, "TotalCells").text = str(total_cells)
        ET.SubElement(summary, "TotalArea_nm2").text = f"{estimate_area_nm2(total_cells):.0f}"
        ET.SubElement(summary, "AreaPerCell_nm2").text = str(AREA_PER_CELL)
        ET.SubElement(summary, "TechnologyNode").text = "7nm"
        
        # Add all modules in a single section
        modules = ET.SubElement(root, "Modules")
        for fn, (nwirebits, ncells) in results:
            module = ET.SubElement(modules, "Module")
            ET.SubElement(module, "Name").text = prettify_name(fn)
            ET.SubElement(module, "WireBits").text = str(nwirebits)
            ET.SubElement(module, "Cells").text = str(ncells)
            ET.SubElement(module, "Area_nm2").text = f"{estimate_area_nm2(ncells):.0f}"
        
        # Create the XML tree and write to file
        # Paths are relative to where script is run from (root directory)
        tree = ET.ElementTree(root)
        output_file = "generated/cell_count_report.xml"
        os.makedirs("generated", exist_ok=True)
        with open(output_file, "wb") as f:
            tree.write(f, encoding='utf-8', xml_declaration=True)
            
        logger.info(f"XML report generated successfully: {output_file}")
    except Exception as e:
        logger.error(f"Error creating XML report: {str(e)}")
        raise

def process_files(files: List[str]) -> Dict[str, Tuple[int, int]]:
    """Process files in parallel using ProcessPoolExecutor."""
    results = {}
    with ProcessPoolExecutor(max_workers=MAX_WORKERS) as executor:
        future_to_file = {executor.submit(yosys, f): f for f in files}
        for future in as_completed(future_to_file):
            file = future_to_file[future]
            try:
                results[file] = future.result()
            except Exception as e:
                logger.error(f"Error processing {file}: {str(e)}")
                results[file] = (0, 0)
    return results

def print_loading_animation():
    """Print a simple loading circle with elapsed time."""
    # Define the loading circle frames
    loading_frames = ['|', '/', '-', '\\']
    
    frame = 0
    start_time = time.time()
    
    while True:
        print("\033[H\033[J")  # Clear screen
        
        # Calculate elapsed time
        elapsed = time.time() - start_time
        
        # Print loading circle and elapsed time
        print(f"Loading {loading_frames[frame]} Time elapsed: {elapsed:.1f} seconds")
        
        # Update frame
        frame = (frame + 1) % len(loading_frames)
        time.sleep(0.1)  # Control animation speed

def main():
    """Main function to process files and generate report."""
    try:
        # Check if Yosys is available
        try:
            result = subprocess.run(['yosys', '-V'], 
                          stdout=subprocess.PIPE, 
                          stderr=subprocess.PIPE, 
                          timeout=5)
            if result.returncode == 0:
                logger.info("Yosys found and ready")
            else:
                logger.error("Yosys is not working properly. Please check your installation.")
                sys.exit(1)
        except FileNotFoundError:
            logger.error("Yosys not found!")
            logger.error("Please install Yosys and ensure it's in your PATH.")
            logger.error("Installation: https://github.com/YosysHQ/yosys")
            sys.exit(1)
        except Exception as e:
            logger.error(f"Could not verify Yosys installation: {e}")
            sys.exit(1)
        
        # Clear Yosys log so we only report on this run (was appending across runs)
        log_file_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'yosys_output.log')
        with open(log_file_path, 'w', encoding='utf-8') as lf:
            lf.write(f"# Yosys log — run started {datetime.now().isoformat()}\n")
        
        # Start the loading animation in a separate thread
        animation_thread = threading.Thread(target=print_loading_animation, daemon=True)
        animation_thread.start()
        
        start_time = datetime.now()
        logger.info("Starting analysis")
        
        # Get list of files to process
        fns = []
        
        # Look for SystemVerilog files in generated directory
        # Paths are relative to where script is run from (root directory)
        sv_files = []
        # Check generated directory structure
        if os.path.exists("generated"):
            sv_files.extend(glob.glob("generated/**/*.sv", recursive=True))
            logger.info(f"Found {len(sv_files)} SystemVerilog files in generated/")
        else:
            # Fallback: look in current directory
            sv_files.extend(glob.glob("CFG_FP_*.sv"))
            sv_files.extend(glob.glob("FP*.sv"))
            sv_files.extend(glob.glob("Rial*.sv"))
            logger.info(f"Found {len(sv_files)} SystemVerilog files in current directory")
        fns.extend(sv_files)
        
        # Look for VHDL files in the current directory
        vhdl_files = glob.glob("*.vhdl")
        if vhdl_files:
            logger.info(f"Found {len(vhdl_files)} VHDL files to process")
            logger.info("VHDL files found:")
            for vhdl in vhdl_files:
                logger.info(f"  - {vhdl}")
            fns.extend(vhdl_files)
        else:
            logger.warning("No VHDL files found in current directory")
        
        if len(sys.argv) >= 2:
            # Use command line argument as glob pattern (like old script)
            fns = glob.glob(sys.argv[1], recursive=True)
            logger.info(f"Using pattern '{sys.argv[1]}': found {len(fns)} files")
        
        if not fns:
            logger.warning("No files found to process")
            return
        
        logger.info(f"Found {len(fns)} total files to process")
        
        # Process files in parallel
        results = process_files(fns)
        
        # Print processing results
        successful_files = []
        failed_files = []
        for fn, (wire_bits, cells) in results.items():
            if cells > 0:
                successful_files.append((fn, cells))
            else:
                failed_files.append(fn)
        
        logger.info(f"Successfully processed {len(successful_files)} files:")
        for fn, cells in successful_files:
            logger.info(f"  - {fn}: {cells} cells")
        
        if failed_files:
            logger.warning(f"Failed to process {len(failed_files)} files:")
            
            # Analyze failure reasons from log file
            error_types = {}
            log_file_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'yosys_output.log')
            if os.path.exists(log_file_path):
                try:
                    with open(log_file_path, 'r', encoding='utf-8') as log_f:
                        log_content = log_f.read()
                        for fn in failed_files:
                            # Find errors for this file in the log
                            fn_base = os.path.basename(fn)
                            pattern = rf'=== Processing {re.escape(fn)} ===.*?Yosys stderr:\n(.*?)(?=\n===|\Z)'
                            match = re.search(pattern, log_content, re.DOTALL)
                            if match:
                                stderr = match.group(1)
                                if 'TOK_AUTOMATIC' in stderr:
                                    error_types['automatic keyword'] = error_types.get('automatic keyword', 0) + 1
                                elif "Can't open include file" in stderr:
                                    error_types['missing include'] = error_types.get('missing include', 0) + 1
                                elif "syntax error" in stderr and "unexpected '['" in stderr:
                                    error_types['unpacked arrays'] = error_types.get('unpacked arrays', 0) + 1
                                elif "syntax error" in stderr:
                                    error_types['other syntax errors'] = error_types.get('other syntax errors', 0) + 1
                except Exception as e:
                    logger.debug(f"Could not analyze error types: {e}")
            
            if error_types:
                logger.warning(f"  Error breakdown:")
                for error_type, count in error_types.items():
                    logger.warning(f"    - {error_type}: {count} files")
            
            logger.warning(f"  Check yosys_output.log for detailed error messages")
            logger.info(f"  Successfully processed {len(successful_files)}/{len(fns)} files ({100*len(successful_files)//len(fns)}%)")
        
        # Create XML report
        create_xml_report(list(results.items()))
        
        # Print summary
        total_cells = sum(cells[1] for _, cells in results.items())
        logger.info(f"Analysis complete. Total cells: {total_cells}")
        logger.info(f"Successfully processed {len(successful_files)} out of {len(fns)} files")
        
        # Print runtime
        end_time = datetime.now()
        runtime = end_time - start_time
        logger.info(f"Total runtime: {runtime}")
        
    except Exception as e:
        logger.error(f"Error in main execution: {str(e)}")
        sys.exit(1)

if __name__ == "__main__":
    # Setup logging first
    logger = setup_logging()
    main()
