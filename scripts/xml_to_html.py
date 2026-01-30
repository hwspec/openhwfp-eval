#!/usr/bin/env python3
"""
Convert cell_count_report.xml to an HTML report with visualizations.
"""

import xml.etree.ElementTree as ET
import sys
import os
from datetime import datetime

def generate_html_report(xml_file: str, output_file: str):
    """Generate HTML report from XML file."""
    
    # Parse XML
    try:
        tree = ET.parse(xml_file)
        root = tree.getroot()
    except FileNotFoundError:
        print(f"Error: XML file not found: {xml_file}")
        sys.exit(1)
    except ET.ParseError as e:
        print(f"Error parsing XML: {e}")
        sys.exit(1)
    
    # Extract data
    timestamp = root.find('Timestamp').text if root.find('Timestamp') is not None else "Unknown"
    summary = root.find('Summary')
    total_cells = summary.find('TotalCells').text if summary.find('TotalCells') is not None else "0"
    total_area = summary.find('TotalArea_nm2').text if summary.find('TotalArea_nm2') is not None else "0"
    area_per_cell = summary.find('AreaPerCell_nm2').text if summary.find('AreaPerCell_nm2') is not None else "100"
    tech_node = summary.find('TechnologyNode').text if summary.find('TechnologyNode') is not None else "7nm"
    
    modules = root.find('Modules')
    module_data = []
    if modules is not None:
        for module in modules.findall('Module'):
            name = module.find('Name').text if module.find('Name') is not None else "Unknown"
            wire_bits = module.find('WireBits').text if module.find('WireBits') is not None else "0"
            cells = module.find('Cells').text if module.find('Cells') is not None else "0"
            area = module.find('Area_nm2').text if module.find('Area_nm2') is not None else "0"
            module_data.append({
                'name': name,
                'wire_bits': int(wire_bits),
                'cells': int(cells),
                'area': float(area)
            })
    
    # Sort by area (largest first)
    module_data.sort(key=lambda x: x['area'], reverse=True)
    
    # Generate HTML
    html_content = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>PPA Analysis Report</title>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}
        
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            padding: 20px;
            min-height: 100vh;
        }}
        
        .container {{
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            border-radius: 12px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            overflow: hidden;
        }}
        
        .header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 40px;
            text-align: center;
        }}
        
        .header h1 {{
            font-size: 2.5em;
            margin-bottom: 10px;
            font-weight: 700;
        }}
        
        .header p {{
            font-size: 1.1em;
            opacity: 0.9;
        }}
        
        .tech-badge {{
            display: inline-block;
            margin-top: 12px;
            padding: 8px 20px;
            background: rgba(255,255,255,0.25);
            border-radius: 20px;
            font-size: 1em;
            font-weight: 600;
            letter-spacing: 0.5px;
        }}
        
        .summary {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            padding: 30px;
            background: #f8f9fa;
        }}
        
        .summary-card {{
            background: white;
            padding: 25px;
            border-radius: 8px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
            text-align: center;
        }}
        
        .summary-card h3 {{
            color: #667eea;
            font-size: 0.9em;
            text-transform: uppercase;
            letter-spacing: 1px;
            margin-bottom: 10px;
        }}
        
        .summary-card .value {{
            font-size: 2.5em;
            font-weight: 700;
            color: #333;
        }}
        
        .summary-card .unit {{
            font-size: 0.9em;
            color: #666;
            margin-left: 5px;
        }}
        
        .content {{
            padding: 30px;
        }}
        
        .section-title {{
            font-size: 1.8em;
            color: #333;
            margin-bottom: 20px;
            padding-bottom: 10px;
            border-bottom: 3px solid #667eea;
        }}
        
        table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 20px;
            background: white;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }}
        
        thead {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
        }}
        
        th {{
            padding: 15px;
            text-align: left;
            font-weight: 600;
            text-transform: uppercase;
            font-size: 0.85em;
            letter-spacing: 0.5px;
        }}
        
        td {{
            padding: 15px;
            border-bottom: 1px solid #e0e0e0;
        }}
        
        tbody tr:hover {{
            background: #f5f5f5;
            transition: background 0.2s;
        }}
        
        tbody tr:last-child td {{
            border-bottom: none;
        }}
        
        .module-name {{
            font-weight: 600;
            color: #667eea;
        }}
        
        .number {{
            font-family: 'Courier New', monospace;
            text-align: right;
        }}
        
        .bar-container {{
            width: 100%;
            height: 20px;
            background: #e0e0e0;
            border-radius: 10px;
            overflow: hidden;
            margin-top: 5px;
        }}
        
        .bar {{
            height: 100%;
            background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
            transition: width 0.3s ease;
        }}
        
        .footer {{
            padding: 20px;
            text-align: center;
            color: #666;
            font-size: 0.9em;
            background: #f8f9fa;
        }}
        
        .category-badge {{
            display: inline-block;
            padding: 4px 12px;
            border-radius: 12px;
            font-size: 0.75em;
            font-weight: 600;
            margin-left: 10px;
        }}
        
        .badge-openfloat {{
            background: #e3f2fd;
            color: #1976d2;
        }}
        
        .badge-hardfloat {{
            background: #fff3e0;
            color: #f57c00;
        }}
        
        .badge-rial {{
            background: #f3e5f5;
            color: #7b1fa2;
        }}
        
        @media (max-width: 768px) {{
            .header h1 {{
                font-size: 1.8em;
            }}
            
            table {{
                font-size: 0.9em;
            }}
            
            th, td {{
                padding: 10px;
            }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>📊 PPA Analysis Report</h1>
            <p>Generated: {timestamp}</p>
            <span class="tech-badge">Area estimates for {tech_node} technology</span>
        </div>
        
        <div class="summary">
            <div class="summary-card">
                <h3>Total Modules</h3>
                <div class="value">{len(module_data)}<span class="unit">modules</span></div>
            </div>
            <div class="summary-card">
                <h3>Total Cells</h3>
                <div class="value">{total_cells}<span class="unit">cells</span></div>
            </div>
            <div class="summary-card">
                <h3>Total Area</h3>
                <div class="value">{float(total_area):,.0f}<span class="unit">nm²</span></div>
            </div>
            <div class="summary-card">
                <h3>Technology Node</h3>
                <div class="value">{tech_node}</div>
            </div>
        </div>
        
        <div class="content">
            <h2 class="section-title">Module Statistics</h2>
            <table>
                <thead>
                    <tr>
                        <th>Module Name</th>
                        <th>Wire Bits</th>
                        <th>Cells</th>
                        <th>Area (nm²)</th>
                        <th>Relative Size</th>
                    </tr>
                </thead>
                <tbody>
"""
    
    # Find max area for bar chart scaling
    max_area = max([m['area'] for m in module_data]) if module_data else 1
    
    for module in module_data:
        name = module['name']
        wire_bits = module['wire_bits']
        cells = module['cells']
        area = module['area']
        bar_width = (area / max_area * 100) if max_area > 0 else 0
        
        # Determine category badge
        badge_class = ""
        badge_text = ""
        if "OpenFloat" in name:
            badge_class = "badge-openfloat"
            badge_text = "OpenFloat"
        elif "HardFloat" in name:
            badge_class = "badge-hardfloat"
            badge_text = "HardFloat"
        elif "Rial" in name:
            badge_class = "badge-rial"
            badge_text = "Rial"
        
        html_content += f"""                    <tr>
                        <td>
                            <span class="module-name">{name}</span>
                            {f'<span class="category-badge {badge_class}">{badge_text}</span>' if badge_text else ''}
                        </td>
                        <td class="number">{wire_bits:,}</td>
                        <td class="number">{cells:,}</td>
                        <td class="number">{area:,.0f}</td>
                        <td>
                            <div class="bar-container">
                                <div class="bar" style="width: {bar_width}%"></div>
                            </div>
                        </td>
                    </tr>
"""
    
    html_content += f"""                </tbody>
            </table>
        </div>
        
        <div class="footer">
            <p>Report generated by PPA Analysis Workflow (Yosys)</p>
            <p>Area estimates based on {area_per_cell} nm² per cell for <strong>{tech_node}</strong> technology</p>
        </div>
    </div>
</body>
</html>
"""
    
    # Write HTML file
    os.makedirs(os.path.dirname(output_file) if os.path.dirname(output_file) else '.', exist_ok=True)
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write(html_content)
    
    print(f"HTML report generated: {output_file}")

def main():
    """Main function."""
    xml_file = "generated/cell_count_report.xml"
    output_file = "generated/ppa_report.html"
    
    if len(sys.argv) >= 2:
        xml_file = sys.argv[1]
    if len(sys.argv) >= 3:
        output_file = sys.argv[2]
    
    if not os.path.exists(xml_file):
        print(f"Error: XML file not found: {xml_file}")
        sys.exit(1)
    
    generate_html_report(xml_file, output_file)

if __name__ == "__main__":
    main()
