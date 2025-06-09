#!/bin/bash
source ~/.bash_profile
cd $detect
rsync --progress data/nifti/*.nii.gz CT_BET/image_data
