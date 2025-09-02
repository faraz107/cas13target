# cas13target


## Getting started

Download the standalone NCBI Blast command line tool from NCBI. The steps for downloading and installing are [here](https://www.ncbi.nlm.nih.gov/books/NBK52640/).

The latest Blast executables can be downloaded from the [FTP server](https://ftp.ncbi.nlm.nih.gov/blast/executables/LATEST/). Download and uncompress the Blast driectory.

Replace the path to downloaded BLAST directory in the `compareHumanRNA.R` function file.

E.g., the following lines set path to "ncbi-blast-2.14.1+".

  `db_path = here("ncbi-blast-2.14.1+", "bin", "rnadb")`
  `blastn_cmd = here("ncbi-blast-2.14.1+", "bin", "blastn")`

## Citation

If you are using this Cas13 guide RNA design tool in your research, please cite: ["Single-base tiled screen unveils design principles of PspCas13b for potent and off-target-free RNA silencing" by Hu et al., Nature Structural & Molecular Biology (2024](https://www.nature.com/articles/s41594-024-01336-0).

## License

GNU GENERAL PUBLIC LICENSE Version 3


