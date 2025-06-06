import os
import pandas as pd

directory = 'simone/4-15-24nicrnova'

for filename in os.listdir(directory):
    if filename.endswith("_frequency.csv"):
        input_file = os.path.join(directory, filename)
        output_file = os.path.join(directory, filename.replace('_frequency.csv', '_classified.csv'))
        
        df = pd.read_csv(input_file)
        
        def classify_sequence(sequence):
            chunks = sequence.split()  # Split the sequence by spaces to get individual chunks
            
            combination = ''
            
            for i, chunk in enumerate(chunks):
                if i == 0:  # chunk1
                    if chunk == 'GATC':
                        combination += 'N'
                    elif chunk == 'GAGT':
                        combination += 'H'
                    elif chunk == 'GTCA':
                        combination += 'Y'
                elif i == 3:  # chunk4
                    if chunk == 'AATG':
                        combination += 'Y'
                    elif chunk == 'ATAC':
                        combination += 'H'
                    elif chunk == 'ACAG':
                        combination += 'N'
                elif i == 6:  # chunk7
                    if chunk == 'CACG':
                        combination += 'Y'
                    elif chunk == 'CCGA':
                        combination += 'H'
                    elif chunk == 'CCAT':
                        combination += 'N'
            
            return combination
        
        df['classification'] = df['sequence'].apply(classify_sequence)
        
        df.to_csv(output_file, index=False)
        
