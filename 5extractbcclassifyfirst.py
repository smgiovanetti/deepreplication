#!/usr/bin/env python3
import pandas as pd

df = pd.read_csv('gene_frequency_extracted.csv')

classifications = {
    'GATCGTATTG': ('rad1neg', 'AAGCACCGGT', 'GATCGTATTG'),
    'ACTCGTATTG': ('ercc4', 'AAGCACCGGT', 'ACTCGTATTG'),
    'TGACGTATTG': ('rad1', 'AAGCACCGGT', 'TGACGTATTG'),
    'CATTACCGGT': ('rad10', 'AAGCGCTAGC', 'CATTACCGGT'),
    'GTATACCGGT': ('ercc1', 'AAGCGCTAGC', 'GTATACCGGT'),
    'CTGTACCGGT': ('rad10neg', 'AAGCGCTAGC', 'CTGTACCGGT'),
    'CGTGGCTAGC': ('rad14', 'TGCACAAAGC', 'CGTGGCTAGC'),
    'TCGGGCTAGC': ('xpa', 'TGCACAAAGC', 'TCGGGCTAGC'),
    'ATGGGCTAGC': ('rad14neg', 'TGCACAAAGC', 'ATGGGCTAGC')
}

def classify_and_extract(row):
    extracted_sequence = str(row['extracted_sequence'])  
    for key, value in classifications.items():
        if key in extracted_sequence:
            classification, before_seq, after_seq = value
            if before_seq in extracted_sequence and after_seq in extracted_sequence:
                start_index = extracted_sequence.index(before_seq) + len(before_seq)
                end_index = extracted_sequence.index(after_seq)
                if start_index < end_index:
                    barcode = extracted_sequence[start_index: end_index]
                    return barcode, key, classification, len(barcode), int(row['frequency']), row['gene']
            else:
                start_index = extracted_sequence.index(key) + len(key)
                end_index = start_index + 10
                if end_index <= len(extracted_sequence):
                    barcode = extracted_sequence[start_index: end_index]
                    return barcode, key, classification, len(barcode), int(row['frequency']), row['gene']
    return None, None, None, None, int(row['frequency']), row['gene']

df['barcode'], df['classification_sequence'], df['classification'], df['barcode_length'], df['frequency'], df['gene'] = zip(*df.apply(classify_and_extract, axis=1))

output_file_with_gene = 'classified_barcodes_with_gene.csv'
output_file_without_gene = 'classified_barcodes_without_gene.csv'

df.to_csv(output_file_with_gene, index=False)

df_without_gene = df.drop(columns=['gene'])
df_without_gene.to_csv(output_file_without_gene, index=False)
