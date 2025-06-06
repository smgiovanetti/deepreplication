import pandas as pd
import os
import glob

input_directory = '/data/SBGE/OneDrive_mongem/NovaSeq Data/NovaSeq Data with Human DNA/MMS NovaSeq Experiment/NICR.barcodes/'
output_directory = '/data/SBGE/simone/4-15-24nicrnova/'  

file_pattern = os.path.join(input_directory, '*.barcodes.txt')
file_list = glob.glob(file_pattern)

headers = ['HC1', 'BC1', 'trash1', 'HC2', 'BC2', 'trash2', 'HC3', 'BC3', 'trash3']

for input_file in file_list:
    base_file_name = os.path.basename(input_file).split('.barcodes.txt')[0]
    df = pd.read_csv(input_file, sep='\t', header=None)
    df.columns = headers
    df['combined'] = df.apply(lambda row: '\t'.join(row.astype(str)), axis=1)
    frequency_table = df.groupby('combined').size().reset_index(name='count')
    frequency_table.drop('combined', axis=1, inplace=True)
    output_file = os.path.join(output_directory, f'{base_file_name}_frequency.csv')
    frequency_table.to_csv(output_file, index=False)
