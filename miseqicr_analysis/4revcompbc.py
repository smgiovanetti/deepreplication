import pandas as pd

df = pd.read_csv('4_collapsed_barcodes.csv')

def reverse_complement(sequence):
    complement_dict = {'A': 'T', 'T': 'A', 'G': 'C', 'C': 'G'}
    return ''.join(complement_dict.get(base, base) for base in reversed(sequence))

df['reverse_complement'] = df['barcode'].apply(reverse_complement)

df.to_csv('4_barcodes_revcomp.csv', index=False)
