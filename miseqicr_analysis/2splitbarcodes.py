import pandas as pd
import re

data = pd.read_csv('4_frequency.csv')

def extract_barcodes(read):
    bc1 = re.search(r'CAATAC(.*?)ACCGGT', read)
    bc2 = re.search(r'ACCGGT(.*?)GCTAGC', read)
    bc3 = re.search(r'GCTAGC(.*?)GCTTTG', read)
    
    bc1 = bc1.group(1) if bc1 else 'x'
    bc2 = bc2.group(1) if bc2 else 'x'
    bc3 = bc3.group(1) if bc3 else 'x'
    
    return pd.Series([bc1, bc2, bc3])

barcodes_df = data['read'].apply(extract_barcodes)
final_df = pd.concat([data, barcodes_df], axis=1)

final_df.to_csv('4_brokenbybarcode.csv', index=False)
final_df.drop(columns=['read'], inplace=True)
final_df.to_csv('4_brokenbybarcode_without_read.csv', index=False)

