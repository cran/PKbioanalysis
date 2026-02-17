import re
import csv

# Read the file
with open('08122019_MTG.txt', 'r', encoding='utf-8') as f:
    lines = f.readlines()

# Parse the data
data = []
for line in lines:
    # Skip header and empty lines
    if not line.strip() or 'Quantify Compound' in line or 'Printed' in line or 'Compound 1:' in line or line.strip().startswith('#'):
        continue
    
    # Split by tab
    parts = [p.strip() for p in line.strip().split('\t')]
    if len(parts) < 3:
        continue
    
    # Extract fields
    try:
        index = parts[0]
        filename = parts[2]
        type_col = parts[3] if len(parts) > 3 else ''
        
        # Determine type - if name contains QC or CS, use the type from file, otherwise 'analyte'
        if 'QC' in filename or 'CS' in filename or type_col in ['Standard', 'QC']:
            sample_type = type_col
        else:
            sample_type = 'analyte'
        
        # Extract subject_id (like R8647, R8648, etc.)
        subject_match = re.search(r'_(R\d+)_', filename)
        subject_id = subject_match.group(1) if subject_match else ''
        
        # Extract time
        time = ''
        if 'PreDose' in filename or 'predose' in filename.lower():
            time = '-1'
        elif 'Min' in filename:
            time_match = re.search(r'_(\d+)Min', filename)
            if time_match:
                time = time_match.group(1)
        elif 'hr' in filename:
            time_match = re.search(r'_(\d+)hr', filename)
            if time_match:
                time = str(int(time_match.group(1)) * 60)  # Convert hours to minutes
        
        # Extract dilution
        dilution = '1X'
        if '_5X' in filename:
            dilution = '5X'
        elif '_10X' in filename:
            dilution = '10X'
        
        data.append({
            'Index': index,
            'filename': filename,
            'Type': sample_type,
            'subject_id': subject_id,
            'Time': time,
            'dilution': dilution
        })
    except Exception as e:
        continue

# Write to CSV
with open('parsed_data.csv', 'w', newline='', encoding='utf-8') as f:
    writer = csv.DictWriter(f, fieldnames=['Index', 'filename', 'Type', 'subject_id', 'Time', 'dilution'])
    writer.writeheader()
    writer.writerows(data)

print(f'Created parsed_data.csv with {len(data)} rows')
