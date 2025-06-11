Rnosave R/02_list_files.R -J FILES --mem=8G -o eofiles/%x_%A.out -e eofiles/%x_%A.err


Rnosave R/03_dump_header.R -J HEADER --array=1-1 --mem=8G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


# Rnosave R/03_dump_header.R -J HEADER --array=1-5109 --mem=8G -o %x_%A_%a.out -e %x_%A_%a.err

sbatch --time=4-00:00:00  -J UNZIP  -o eofiles/%x_%A.out -e eofiles/%x_%A.err R/unzip_data.sh


Rnosave R/04_group_header.R -J GROUPED  --mem=25G -o eofiles/%x_%A.out -e eofiles/%x_%A.err

Rnosave R/05_dicom_to_nifti.R -J NIFTI -x compute-096 --array=1-200 --mem=8G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


Rnosave R/tmp_unzip.R -J UNZ --array=1-2 --mem=4G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


Rnosave R/06_skull_strip.R -J SS --array=113,189 --mem=20G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

sbatch R/run_ct_bet_cpu.sh

Rnosave R/06_plot_nifti.R -J PLOT --array=1-200 --mem=12G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

Rnosave R/06_get_image_dimensions.R -J DIMS --mem=12G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err


Rnosave R/08_plot_skull_strip.R  -x compute-127 -J SPLOT --array=1-200 --mem=12G -o eofiles/%x_%A_%a.out -e eofiles/%x_%A_%a.err

sbatch R/hd_ct_bet.sh

sbatch R/run_ct_bet_cpu.sh
