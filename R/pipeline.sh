Rnosave R/02_list_files.R -J FILES --mem=8G -o %x_%A.out -e %x_%A.err


Rnosave R/03_dump_header.R -J HEADER --array=1-5 --mem=8G -o %x_%A_%a.out -e %x_%A_%a.err


# Rnosave R/03_dump_header.R -J HEADER --array=1-5109 --mem=8G -o %x_%A_%a.out -e %x_%A_%a.err
