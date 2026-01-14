
from pymzml.run import Reader 

# read single mrm 

def read_mrm(file_path):
    run = Reader(file_path)
    mrm_list = run.info["offset_dict"]
    for spectrum in run:
        print(spectrum)