#!/bin/bash
#SBATCH --job-name=HDCTBET
#SBATCH --partition=gpu          # <-- only GPU workloads belong here
#SBATCH --gres=gpu:1             # 1 GPU; change to :2, :4 … if the code scales
#SBATCH --cpus-per-task=4        # threads available to PyTorch / nnUNet
#SBATCH --mem=12G
#SBATCH --time=1-00:00:00
#SBATCH --output=BET_%A.out
#SBATCH --error=BET_%A.err

source ~/.bash_profile
cd $detect
 # [1] "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f"
# files=`ls data/nifti/ID-0*.nii.gz data/nifti/ID-1*.nii.gz data/nifti/ID-2*.nii.gz`
# files=`ls data/nifti/ID-3*.nii.gz data/nifti/ID-4*.nii.gz data/nifti/ID-5*.nii.gz`
# files=`ls data/nifti/ID-6*.nii.gz data/nifti/ID-7*.nii.gz data/nifti/ID-8*.nii.gz`
# files=`ls data/nifti/ID-9*.nii.gz data/nifti/ID-a*.nii.gz data/nifti/ID-b*.nii.gz`
# files=`ls data/nifti/ID-c*.nii.gz data/nifti/ID-d*.nii.gz data/nifti/ID-e*.nii.gz`
files=`ls data/nifti/ID-f*.nii.gz`
outdir="data/brain_extracted_hdctbet"
mkdir -p ${outdir}

conda activate hd_ctbet
for ifile in $files;
do
  bn=`basename ${ifile}`
  outfile="${outdir}/${bn}"
  if [ ! -f ${outfile} ]; 
  then
    ./HD-CTBET/HD_CTBET/hd-ctbet -i ${ifile} -o "${outdir}/${bn}"
  else 
    echo "${outfile}"
  fi
done
