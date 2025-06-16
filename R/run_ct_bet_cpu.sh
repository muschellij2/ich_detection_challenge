#!/bin/bash
#SBATCH --job-name=CTBET
#SBATCH --time=10-00:00:00
#SBATCH --output=CTBET_%A.out
#SBATCH --error=CTBET_%A.err

source ~/.bash_profile
module unload conda_R
module load conda
conda activate ctbet_env
cd $detect
cd CT_BET
python unet_CT_SS.py
