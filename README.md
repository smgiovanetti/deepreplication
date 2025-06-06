ICR plasmids were iniitally sequenced via pac bio long read sequencing.
to extract the barcode and check that the sequence of the associated cargo is correct, the scripts found in pacbio_analysis were used.
scripts are ordered numerically.

the overall process was:

1- collapse all reads to a frequency table (count all identical reads)

2- from frequency table, extract barcode

3- only keep rows (reads) for which a barcode could be extracted

4- extract the associated gene cargo using anchors for each row in frequency table

5- classify gene bsaed on specific sequence of 4 nt (with 6 nt that are specific to the set)

6- further collapse frequency table for rows that have matching barode / barcode length / classification

7- subset


THEN
match gene to correct sequence

8- match gene for perfect sequence, keep barcodes that are correct and incorrect (in separate csv)

9- collapse frequency table a final time for barcodes that meet requirements and are paired with cargo with correct sequence
