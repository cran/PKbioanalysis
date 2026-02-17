import re
import csv

def parse_mtg_file(input_file, output_file):
    """Parse MTG file and create CSV with specified columns."""
    
    data = []
    
    with open(input_file, 'r', encoding='utf-8') as f:
        lines = f.readlines()
    
    # Find the header line
    header_idx = None
    for idx, line in enumerate(lines):
        if line.strip().startswith('#\tName\tType'):
            header_idx = idx
            break
    
    if header_idx is None:
        print("Could not find header line")
        return
    
    # Parse data lines
    for line in lines[header_idx + 1:]:
        # Skip empty lines or lines that don't start with a number
        line = line.strip()
        if not line or not re.match(r'^\d+\t', line):
            continue
        
        parts = line.split('\t')
        if len(parts) < 3:
            continue
        
        # Extract basic fields
        index = parts[0]
        filename = parts[1]
        type_field = parts[2]
        
        # Determine Type: if filename contains "QC" or "CS", use type_field; otherwise "analyte"
        if 'QC' in filename or 'CS' in filename:
            parsed_type = type_field if type_field else "Standard"
        else:
            parsed_type = "analyte"
        
        # Extract subject_id (e.g., R8647)
        subject_match = re.search(r'[_]([R]\d+)[_]', filename)
        subject_id = subject_match.group(1) if subject_match else ""
        
        # Extract time
        time = ""
        if 'PreDose' in filename or 'Predose' in filename:
            time = "-1"
        else:
            # Look for time patterns like "5Min", "10Min", "15Min", etc.
            time_match = re.search(r'[_](\d+)Min', filename, re.IGNORECASE)
            if time_match:
                time = time_match.group(1)
            else:
                # Look for other time patterns
                time_match = re.search(r'[_](\d+)min', filename, re.IGNORECASE)
                if time_match:
                    time = time_match.group(1)
        
        # Extract dilution
        dilution = "1X"
        dilution_match = re.search(r'[_](\d+X)', filename, re.IGNORECASE)
        if dilution_match:
            dilution = dilution_match.group(1)
        
        data.append({
            'Index': index,
            'filename': filename,
            'Type': parsed_type,
            'subject_id': subject_id,
            'Time': time,
            'dilution': dilution
        })
    
    # Write to CSV
    with open(output_file, 'w', newline='', encoding='utf-8') as f:
        fieldnames = ['Index', 'filename', 'Type', 'subject_id', 'Time', 'dilution']
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        
        writer.writeheader()
        writer.writerows(data)
    
    print(f"Parsed {len(data)} rows and saved to {output_file}")

if __name__ == "__main__":
    input_file = "08122019_MTG.txt"
    output_file = "08122019_MTG_parsed.csv"
    parse_mtg_file(input_file, output_file)
