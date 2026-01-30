#!/usr/bin/env python3
"""
Show detailed Yosys errors from the log file.
"""

import re
import sys
import os

def extract_errors(log_file="yosys_output.log"):
    """Extract and display errors from Yosys log."""
    if not os.path.exists(log_file):
        print(f"Error: {log_file} not found")
        return
    
    with open(log_file, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Find all processing sections
    pattern = r'=== Processing (.*?) ===(.*?)(?=\n=== Processing|\Z)'
    matches = re.finditer(pattern, content, re.DOTALL)
    
    errors_by_file = {}
    errors_by_type = {}
    
    for match in matches:
        file_path = match.group(1).strip()
        section = match.group(2)
        
        # Extract stderr section
        stderr_match = re.search(r'Yosys stderr:\n(.*?)(?=\n===|\Z)', section, re.DOTALL)
        if stderr_match:
            stderr = stderr_match.group(1).strip()
            if stderr and 'ERROR' in stderr:
                # Extract error lines
                error_lines = [line.strip() for line in stderr.split('\n') if 'ERROR' in line]
                if error_lines:
                    errors_by_file[file_path] = error_lines
                    
                    # Categorize errors
                    for error in error_lines:
                        if 'TOK_AUTOMATIC' in error or 'automatic' in error.lower():
                            errors_by_type.setdefault('automatic keyword', []).append(file_path)
                        elif "unexpected '['" in error or 'OP_CAST' in error or 'unpacked' in error.lower():
                            errors_by_type.setdefault('unpacked arrays/syntax', []).append(file_path)
                        elif "Invalid nesting" in error:
                            errors_by_type.setdefault('invalid nesting', []).append(file_path)
                        elif "Can't open include" in error:
                            errors_by_type.setdefault('missing include', []).append(file_path)
                        else:
                            errors_by_type.setdefault('other', []).append(file_path)
    
    # Display results
    print("=" * 80)
    print("Yosys Error Analysis")
    print("=" * 80)
    print()
    
    if errors_by_type:
        print("Errors by Type:")
        print("-" * 80)
        for error_type, files in errors_by_type.items():
            unique_files = list(set(files))  # Remove duplicates
            print(f"\n{error_type}: {len(unique_files)} files")
            for f in unique_files[:10]:  # Show first 10
                print(f"  - {os.path.basename(f)}")
            if len(unique_files) > 10:
                print(f"  ... and {len(unique_files) - 10} more")
        print()
    
    print("=" * 80)
    print("Detailed Errors by File:")
    print("=" * 80)
    print()
    
    # Show detailed errors for each file (limit to first 20)
    for i, (file_path, error_lines) in enumerate(list(errors_by_file.items())[:20]):
        print(f"\n{os.path.basename(file_path)}:")
        for error in error_lines[:3]:  # Show first 3 errors per file
            print(f"  {error}")
        if len(error_lines) > 3:
            print(f"  ... and {len(error_lines) - 3} more errors")
    
    if len(errors_by_file) > 20:
        print(f"\n... and {len(errors_by_file) - 20} more files with errors")
    
    print()
    print("=" * 80)
    print(f"Total files with errors: {len(errors_by_file)}")
    print("=" * 80)

if __name__ == "__main__":
    log_file = sys.argv[1] if len(sys.argv) > 1 else "yosys_output.log"
    extract_errors(log_file)
