#!/usr/bin/env python3

import gzip
import pandas as pd 
import os

FASTQ file
input_file = '/data/SBGE/molly/PACBIO/pacbio.ICR/m64467e_230327_154324.hifi_reads.fastq.gz'

base_file_name = os.path.basename(input_file).split('.')[0]

reads = []

with gzip.open(input_file, 'rt') as fastq_file:
    for line_num, line in enumerate(fastq_file):
        if line_num % 4 == 1:
            reads.append(line.strip())

df = pd.DataFrame({'read': reads})

frequency_table = df['read'].value_counts().reset_index()

frequency_table.columns = ['read', 'frequency']

output_file = f'{base_file_name}_frequency.csv'
frequency_table.to_csv(output_file, index=False)

