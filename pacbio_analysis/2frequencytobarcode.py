#!/usr/bin/env python3
#for extracting barcode containing region
import re
import pandas as pd
import os
from Bio.Seq import Seq

#anchors
anchor1 = 'GTTCCTAGTGTGCACAAAGC'  
anchor2 = 'GTATTGCGACGAATTGCCAC'  

# input
input_csv_file = 'm64467e_230327_154324_frequency.csv'  

base_file_name = os.path.basename(input_csv_file).split('.')[0]

df = pd.read_csv(input_csv_file)

extracted_sequences = []
extracted_rc_sequences = []
contains_anchors = []

def extract_sequences_between_anchors(seq, anchor1, anchor2):
    match1 = re.search(f'{anchor1}(.*?){anchor2}', seq)
    if match1:
        seq1 = match1.group(0)  
        contains_anchor = True
    else:
        seq1 = None
        contains_anchor = False

    rc_anchor1 = str(Seq(anchor1).reverse_complement())
    rc_anchor2 = str(Seq(anchor2).reverse_complement())

    match2 = re.search(f'{rc_anchor1}(.*?){rc_anchor2}', seq)
    if match2:
        seq2 = match2.group(0)  
        contains_anchor = True
    else:
        seq2 = None

    return seq1, seq2, contains_anchor

for index, row in df.iterrows():
    extracted_sequence, extracted_rc_sequence, contains_anchor = extract_sequences_between_anchors(row['read'], anchor1, anchor2)
    extracted_sequences.append(extracted_sequence)
    extracted_rc_sequences.append(extracted_rc_sequence)
    contains_anchors.append(contains_anchor)

df['extracted_sequence'] = extracted_sequences
df['extracted_rc_sequence'] = extracted_rc_sequences
df['contains_anchors'] = contains_anchors

output_file = f'{base_file_name}_extracted_sequences.csv'
df.to_csv(output_file, index=False)
