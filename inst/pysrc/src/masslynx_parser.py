#%%
import os 
import pandas as pd
import rainbow as rb
import numpy as np 


#%%
def get_chromatograms(path):
    """
    Read the waters data files and return a list of dataframes

    Args:
    pathdir: str
        The path to the directory containing the waters data files
    
    """
    # path = "E:/raw/2024-04-02_EE_in30__uD_R18.raw"
    datadir  = rb.read(path)
    # for name in datadir.by_name:
        # BUG I will only use FUNC001.DAT for now. Other files like CHRO001.DAT and CHRO002.DAT in NEU are not used
    datafile = datadir.get_file("_FUNC001.DAT") # 
    times = datafile.xlabels
    transitions = datafile.ylabels
    intensity = datafile.data


    # just skip transition names for now
    df = pd.DataFrame(intensity, columns = transitions)
    df["RT"] = times
    df["sample"] = os.path.basename(path)
    return df




def parse_compound_names(path):
    #     data = f.read().decode("ISO-8859-1").encode("latin1").decode("ascii", "ignore")
    path = os.path.join(path, '_FUNC001.CMP')
    dts = np.dtype([('compounds', 'S256'), ('transition', 'S256'), ('source', 'S512')])
    dtu = np.dtype([('compounds', 'U256'), ('transition', 'U256'), ('source', 'U512')])

    df = pd.DataFrame(np.fromfile(path, dtype=dts, offset=12).astype(dtu))
    # df['measurements'] = df['compounds'] + ' ' + (df.index.values + 1).astype(str)

    return df

def parse_metadata(path):
    """
    Adopted from rainbowapi
    Parses metadata from a Waters .raw directory.

    Specifically, the date and vial position are extracted from _HEADER.txt.

    Args:
        path (str): Path to the .raw directory. 
    
    Returns:
        Dictionary with directory metadata. 

    """
    metadata = {}
    metadata['vendor'] = "Waters"

    with open(os.path.join(path, '_HEADER.TXT'), 'r') as f:
        lines = f.read().splitlines()
    for line in lines:
        if line.startswith("$$ Acquired Date"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['date'] = value + " "
        elif line.startswith("$$ Acquired Time"):
            # assert('date' in metadata)
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['date'] += value
        elif line.startswith("$$ Bottle Number"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['vialpos'] = value
        elif line.startswith("$$ Instrument"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['instrument'] = value
        elif line.startswith("$$ Job Code"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['job_name'] = value
        elif line.startswith("$$ Sample Description"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['sample_description'] = value

    # # parse the _INLET.INF file
    with open(os.path.join(path, '_INLET.INF'), 'r', encoding="latin-1") as f:
        lines = f.read().splitlines()
    for line in lines:
        if line.startswith(" Injection Mode"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['injection_mode'] = value
        elif line.startswith("Injection Volume"):
            value = line.split('- ')[1]
            if not value.isspace():
                metadata['injection_volume'] = value
        elif line.startswith(" Column Type"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['column_type'] = value
        elif line.startswith(" Column Serial Number"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['column_serial_number'] = value
        elif line.startswith("Total Injections on Column"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['total_injections_on_column'] = value
        elif line.startswith(" Run Time"):
            value = line.split(': ')[1]
            if not value.isspace():
                metadata['run_time'] = value
    return metadata



# %%
def parse_funcinf_q3(path):
    """
    Parses a Waters _FUNCTNS.INF file for Q3 transitions

    This file contains mz values for the 2-byte format. 

    Learn more about this file format :ref:`here <funcdat2>`.

    Args:
        path (str): Path to the _FUNCTNS.INF file. 

    Returns:
        2D numpy array of mz values where the rows correspond to functions.

    """
    with open(path, 'rb') as f:
        raw_bytes = f.read()
    num_funcs = os.path.getsize(path) // 416
    mzs = np.ndarray((num_funcs, 32), "<f", raw_bytes, 288, (416, 4))
    mzs = mzs[mzs != 0]
    return mzs


#%%
def parse_funcinf(path):
    """
    Parses a Waters _FUNCTNS.INF file. 

    This file contains mz values for the 2-byte format. 

    Learn more about this file format :ref:`here <funcdat2>`.

    Args:
        path (str): Path to the _FUNCTNS.INF file. 

    Returns:
        2D numpy array of mz values where the rows correspond to functions.

    """
    with open(path, 'rb') as f:
        raw_bytes = f.read()
    num_funcs = os.path.getsize(path) // 416
    mzs = np.ndarray((num_funcs, 32), "<f", raw_bytes, 160, (416, 4))
    mzs = mzs[mzs != 0]
    return mzs

#%% 
def read_waters(path):
    chromadf = get_chromatograms(path)
    cmpddf = parse_compound_names(path) # ignore transitions inside this 
    metedata = parse_metadata(path)
    q1 = parse_funcinf(os.path.join(path, '_FUNCTNS.INF'))
    q3 = parse_funcinf_q3(os.path.join(path, '_FUNCTNS.INF'))
    return [chromadf, cmpddf, metedata, q1, q3]
# %%
