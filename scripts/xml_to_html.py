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
    if summary is None:
        total_cells = "0"
        total_area = "0"
        area_per_cell = "100"
        tech_node = "7nm"
    else:
        total_cells = summary.find('TotalCells').text if summary.find('TotalCells') is not None else "0"
        total_area = summary.find('TotalArea_nm2').text if summary.find('TotalArea_nm2') is not None else "0"
        el = summary.find('AreaPerCell_nm2')
        area_per_cell = el.text if el is not None and el.text else "100"
        el = summary.find('TechnologyNode')
        tech_node = el.text if el is not None and el.text else "7nm"
    
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
    
    # Assign library (tab) from path
    def get_lib(full_name: str) -> str:
        n = full_name.lower()
        if "openfloat" in n:
            return "openfloat"
        if "hardfloat" in n:
            return "hardfloat"
        if "rial" in n:
            return "rial"
        return "other"

    for m in module_data:
        m['lib'] = get_lib(m['name'])

    # Sort all by area (largest first)
    module_data.sort(key=lambda x: x['area'], reverse=True)

    # Group by library for tabs (each group sorted by area)
    lib_order = ("openfloat", "hardfloat", "rial", "other")
    by_lib = {}
    for lib in lib_order:
        by_lib[lib] = [m for m in module_data if m['lib'] == lib]
    by_lib["all"] = list(module_data)

    # Format total cells with commas
    total_cells_int = int(total_cells)
    total_area_float = float(total_area)
    total_area_mm2 = total_area_float / 1e6  # Convert nm² to mm²

    # Footer line (built here so f-string interpolation is guaranteed)
    footer_area_line = f"Yosys Synthesis Estimate | Area: {area_per_cell} nm²/cell ({tech_node} assumption)"

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
            background: #1a1a2e;
            padding: 20px;
            min-height: 100vh;
        }}

        .container {{
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            border-radius: 4px;
            box-shadow: 0 4px 20px rgba(0,0,0,0.4);
            overflow: hidden;
        }}

        .header {{
            background: #16213e;
            color: #e8e8e8;
            padding: 40px;
            text-align: center;
            border-bottom: 2px solid #0f3460;
        }}

        .header h1 {{
            font-size: 2.2em;
            margin-bottom: 10px;
            font-weight: 700;
        }}

        .header p {{
            font-size: 1em;
            opacity: 0.9;
        }}

        .tech-badge {{
            display: inline-block;
            margin-top: 12px;
            padding: 6px 16px;
            background: #0f3460;
            border: 1px solid #e94560;
            border-radius: 4px;
            font-size: 0.85em;
            font-weight: 500;
            letter-spacing: 0.5px;
            color: #e94560;
        }}

        .estimate-notice {{
            display: block;
            margin-top: 8px;
            font-size: 0.9em;
            color: #a0a0a0;
            font-style: italic;
        }}

        .summary {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
            gap: 15px;
            padding: 25px;
            background: #f5f5f5;
        }}

        .summary-card {{
            background: white;
            padding: 20px;
            border-radius: 4px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
            text-align: center;
            border-left: 3px solid #0f3460;
        }}

        .summary-card h3 {{
            color: #0f3460;
            font-size: 0.8em;
            text-transform: uppercase;
            letter-spacing: 1px;
            margin-bottom: 8px;
            font-weight: 600;
        }}

        .summary-card .value {{
            font-size: 1.6em;
            font-weight: 700;
            color: #16213e;
            font-family: 'Courier New', monospace;
        }}

        .summary-card .unit {{
            font-size: 0.8em;
            color: #666;
            margin-left: 4px;
        }}

        .content {{
            padding: 25px;
        }}

        .section-title {{
            font-size: 1.3em;
            color: #16213e;
            margin-bottom: 15px;
            padding-bottom: 8px;
            border-bottom: 2px solid #0f3460;
            font-weight: 600;
        }}

        table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
            background: white;
            font-size: 0.9em;
        }}

        thead {{
            background: #16213e;
            color: #e8e8e8;
        }}

        th {{
            padding: 12px 15px;
            text-align: left;
            font-weight: 600;
            text-transform: uppercase;
            font-size: 0.8em;
            letter-spacing: 0.5px;
        }}

        td {{
            padding: 10px 15px;
            border-bottom: 1px solid #e0e0e0;
        }}

        tbody tr:hover {{
            background: #f8f8f8;
        }}

        tbody tr:last-child td {{
            border-bottom: none;
        }}

        .module-name {{
            font-weight: 600;
            color: #0f3460;
            font-family: 'Courier New', monospace;
            font-size: 0.85em;
        }}

        .number {{
            font-family: 'Courier New', monospace;
            text-align: right;
        }}

        .footer {{
            padding: 15px;
            text-align: center;
            color: #555;
            font-size: 0.8em;
            background: #f0f0f0;
            border-top: 1px solid #ddd;
        }}

        .category-badge {{
            display: inline-block;
            padding: 3px 8px;
            border-radius: 3px;
            font-size: 0.7em;
            font-weight: 600;
            margin-left: 8px;
        }}

        .badge-openfloat {{
            background: #1a3a5c;
            color: #4da6ff;
        }}

        .badge-hardfloat {{
            background: #3d2c1a;
            color: #ffb74d;
        }}

        .badge-rial {{
            background: #2d1a3d;
            color: #ce93d8;
        }}

        .tabs {{
            display: flex;
            gap: 4px;
            margin-bottom: 20px;
            border-bottom: 2px solid #e0e0e0;
            padding-bottom: 0;
        }}

        .tab-btn {{
            padding: 10px 20px;
            border: none;
            background: #e8e8e8;
            color: #333;
            font-size: 0.95em;
            font-weight: 600;
            cursor: pointer;
            border-radius: 4px 4px 0 0;
            transition: background 0.2s, color 0.2s;
        }}

        .tab-btn:hover {{
            background: #d0d0d0;
        }}

        .tab-btn.active {{
            background: #16213e;
            color: #e8e8e8;
            border-bottom: 2px solid #16213e;
            margin-bottom: -2px;
        }}

        .tab-panel {{
            display: none;
        }}

        .tab-panel.active {{
            display: block;
        }}

        .tab-panel .tab-count {{
            font-size: 0.85em;
            color: #666;
            margin-left: 8px;
            font-weight: normal;
        }}

        .tool-tabs {{
            display: flex;
            gap: 0;
            padding: 0 25px;
            background: #e0e0e0;
            border-bottom: 2px solid #0f3460;
        }}

        .tool-tab {{
            padding: 12px 24px;
            border: none;
            background: transparent;
            color: #555;
            font-size: 1em;
            font-weight: 600;
            cursor: pointer;
            border-bottom: 3px solid transparent;
            margin-bottom: -2px;
            transition: background 0.2s, color 0.2s;
        }}

        .tool-tab:hover {{
            background: #d0d0d0;
            color: #333;
        }}

        .tool-tab.active {{
            background: white;
            color: #16213e;
            border-bottom-color: #16213e;
        }}

        .tool-content {{
            display: none;
        }}

        .tool-content.active {{
            display: block;
        }}

        .coming-soon {{
            text-align: center;
            padding: 60px 25px;
            color: #666;
        }}

        .coming-soon h3 {{
            font-size: 1.5em;
            color: #16213e;
            margin-bottom: 12px;
        }}

        .coming-soon p {{
            max-width: 480px;
            margin: 0 auto;
            line-height: 1.5;
        }}

        @media (max-width: 768px) {{
            .header h1 {{ font-size: 1.5em; }}
            table {{ font-size: 0.8em; }}
            th, td {{ padding: 8px; }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>PPA Analysis Report</h1>
            <p>Generated: {timestamp}</p>
            <span class="tech-badge">{tech_node} Technology Node</span>
            <span class="estimate-notice">Preliminary estimates via Yosys synthesis (no PDK)</span>
        </div>

        <div class="summary">
            <div class="summary-card">
                <h3>Total Modules</h3>
                <div class="value">{len(module_data)}<span class="unit">modules</span></div>
            </div>
            <div class="summary-card">
                <h3>Total Cells</h3>
                <div class="value">{total_cells_int:,}<span class="unit">cells</span></div>
            </div>
            <div class="summary-card">
                <h3>Total Area</h3>
                <div class="value">{total_area_mm2:.1f}<span class="unit">mm²</span></div>
            </div>
            <div class="summary-card">
                <h3>Technology</h3>
                <div class="value">{tech_node}</div>
            </div>
        </div>

        <div class="tool-tabs" role="tablist">
            <button class="tool-tab active" data-tool="yosys" role="tab" aria-selected="true">Yosys</button>
            <button class="tool-tab" data-tool="openroad" role="tab" aria-selected="false">OpenROAD</button>
        </div>

        <div class="tool-content active" id="tool-yosys" role="tabpanel">
        <div class="content">
            <h2 class="section-title">Module Statistics (Yosys)</h2>
            <div class="tabs" role="tablist">
                <button class="tab-btn active" data-tab="all" role="tab" aria-selected="true">All</button>
                <button class="tab-btn" data-tab="openfloat" role="tab" aria-selected="false">OpenFloat</button>
                <button class="tab-btn" data-tab="hardfloat" role="tab" aria-selected="false">HardFloat</button>
                <button class="tab-btn" data-tab="rial" role="tab" aria-selected="false">Rial</button>
            </div>
"""

    def row_html(module):
        full_name = module['name']
        wire_bits = module['wire_bits']
        cells = module['cells']
        area = module['area']
        display_name = full_name
        for prefix in ("generated/rial/", "generated/openfloat/", "generated/hardfloat/"):
            if full_name.startswith(prefix):
                display_name = full_name[len(prefix):]
                break
        badge_class = ""
        badge_text = ""
        if "openfloat" in full_name.lower():
            badge_class = "badge-openfloat"
            badge_text = "OpenFloat"
        elif "hardfloat" in full_name.lower():
            badge_class = "badge-hardfloat"
            badge_text = "HardFloat"
        elif "rial" in full_name.lower():
            badge_class = "badge-rial"
            badge_text = "Rial"
        badge_html = f'<span class="category-badge {badge_class}">{badge_text}</span>' if badge_text else ''
        return f"""                    <tr>
                        <td><span class="module-name">{display_name}</span>{badge_html}</td>
                        <td class="number">{wire_bits:,}</td>
                        <td class="number">{cells:,}</td>
                        <td class="number">{area:,.0f}</td>
                    </tr>
"""

    tab_labels = {"all": "All", "openfloat": "OpenFloat", "hardfloat": "HardFloat", "rial": "Rial"}
    for tab_id in ("all", "openfloat", "hardfloat", "rial"):
        rows = by_lib.get(tab_id, [])
        active = " active" if tab_id == "all" else ""
        count = len(rows)
        html_content += f"""
            <div class="tab-panel{active}" id="panel-{tab_id}" role="tabpanel" aria-labelledby="tab-{tab_id}">
                <table>
                    <thead>
                        <tr>
                            <th>Module Name</th>
                            <th>Wire Bits</th>
                            <th>Cells</th>
                            <th>Area (nm²)</th>
                        </tr>
                    </thead>
                    <tbody>
"""
        for module in rows:
            html_content += row_html(module)
        html_content += """                    </tbody>
                </table>
            </div>
"""

    html_content += """
        </div>
        </div>

        <div class="tool-content" id="tool-openroad" role="tabpanel">
            <div class="coming-soon">
                <h3>OpenROAD — Coming soon</h3>
                <p>PPA results from OpenROAD place-and-route will appear here once generated reports (e.g. from OpenROAD-generated XML) are available. You will be able to compare Yosys estimates with OpenROAD results side by side.</p>
                <p style="color:#888; font-size:0.9em; margin-top:12px;">Uncertain which PDK will be used yet; may test multiple PDKs (e.g. 45nm, ASAP7) and add results as they are available.</p>
            </div>
        </div>

        <div class="footer" id="footer-yosys-only">
            <p>FOOTER_AREA_LINE</p>
            <p style="color:#888; font-size:0.75em; margin-top:4px;">Note: Preliminary estimates without PDK. Actual silicon area will vary.</p>
        </div>
    </div>
    <script>
        document.querySelectorAll('.tool-tab').forEach(function(btn) {{
            btn.addEventListener('click', function() {{
                var tool = this.getAttribute('data-tool');
                document.querySelectorAll('.tool-tab').forEach(function(b) {{ b.classList.remove('active'); b.setAttribute('aria-selected', 'false'); }});
                document.querySelectorAll('.tool-content').forEach(function(p) {{ p.classList.remove('active'); }});
                this.classList.add('active');
                this.setAttribute('aria-selected', 'true');
                var panel = document.getElementById('tool-' + tool);
                if (panel) panel.classList.add('active');
                var footer = document.getElementById('footer-yosys-only');
                if (footer) footer.style.display = (tool === 'yosys') ? '' : 'none';
            }});
        }});
        document.querySelectorAll('.tab-btn').forEach(function(btn) {{
            btn.addEventListener('click', function() {{
                var tab = this.getAttribute('data-tab');
                document.querySelectorAll('.tab-btn').forEach(function(b) {{ b.classList.remove('active'); b.setAttribute('aria-selected', 'false'); }});
                document.querySelectorAll('.tab-panel').forEach(function(p) {{ p.classList.remove('active'); }});
                this.classList.add('active');
                this.setAttribute('aria-selected', 'true');
                var panel = document.getElementById('panel-' + tab);
                if (panel) panel.classList.add('active');
            }});
        }});
    </script>
</body>
</html>
"""
    
    html_content = html_content.replace("FOOTER_AREA_LINE", footer_area_line)

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
