#!/usr/bin/env python3

import gzip
import pandas as pd 
import os

input_file = '4.assembled.fastq'

base_file_name = os.path.basename(input_file).split('.')[0]

reads = []

with open(input_file, 'r') as fastq_file:
    for line_num, line in enumerate(fastq_file):
        if line_num % 4 == 1:
            reads.append(line.strip())

df = pd.DataFrame({'read': reads})
frequency_table = df['read'].value_counts().reset_index()
frequency_table.columns = ['read', 'count']
output_file = f'{base_file_name}_frequency.csv'
frequency_table.to_csv(output_file, index=False)
