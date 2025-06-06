import pandas as pd

df = pd.read_csv('4_frequency.csv')

classifications = {
    'GATCGTATTG': ('rad1neg', 'CAATACGATC', 'ACCGGTGCTT'),
    'ACTCGTATTG': ('ercc4', 'CAATACGAGT', 'ACCGGTGCTT'),
    'TGACGTATTG': ('rad1', 'CAATACGTCA', 'ACCGGTGCTT'),
    'CATTACCGGT': ('rad10', 'ACCGGTAATG', 'GCTAGCGCTT'),
    'GTATACCGGT': ('ercc1', 'ACCGGTATAC', 'GCTAGCGCTT'),
    'CTGTACCGGT': ('rad10neg', 'ACCGGTACAG', 'GCTAGCGCTT'),
    'CGTGGCTAGC': ('rad14', 'GCTAGCCACG', 'GCTTTGTGCA'),
    'TCGGGCTAGC': ('xpa', 'GCTAGCCCGA', 'GCTTTGTGCA'),
    'ATGGGCTAGC': ('rad14neg', 'GCTAGCCCAT', 'GCTTTGTGCA')
}

def extract_barcode(row):
    read = str(row['read'])
    for anchor, (classification, start_anchor, end_anchor) in classifications.items():
        if start_anchor in read and end_anchor in read:
            start_index = read.index(start_anchor) + len(start_anchor)
            end_index = read.index(end_anchor)
            barcode = read[start_index:end_index]
            return barcode, classification
    return None, None

df['barcode'], df['classification'] = zip(*df.apply(extract_barcode, axis=1))

matched_df = df[df['classification'].notna()]
matched_df[['count', 'barcode', 'classification']].to_csv('4_frequency_with_barcodes.csv', index=False)
unmatched_df = df[df['classification'].isna()]
unmatched_df[['read', 'count']].to_csv('4_unmatched_reads.csv', index=False)
