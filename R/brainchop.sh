#!/bin/bash
#SBATCH --job-name=BRAINCHOP
#SBATCH --partition=gpu          # <-- only GPU workloads belong here
#SBATCH --gres=gpu:1             # 1 GPU; change to :2, :4 … if the code scales
#SBATCH --cpus-per-task=4        # threads available to PyTorch / nnUNet
#SBATCH --mem=12G
#SBATCH --time=1-00:00:00
#SBATCH --output=BRAINCHOP_%A.out
#SBATCH --error=BRAINCHOP_%A.err

# module unload conda_R || true
# module unload freesurfer || true
# module unload fsl || true
# module load conda
# conda create -n brainchop_env python=3.11
# conda activate brainchop
# uv pip install brainchop

source ~/.bash_profile
module unload conda_R || true
module unload freesurfer || true
module unload fsl || true
module load conda
conda activate brainchop_env
cd $detect

# files=`ls data/nifti/ID-8*.nii.gz data/nifti/ID-9*.nii.gz`
files="/dcs05/ciprian/smart/ich_detection_challenge/data/nifti/ID-e05a3ff4_ID-b1ba4ce57e.nii.gz /dcs05/ciprian/smart/ich_detection_challenge/data/nifti/ID-e056b05b_ID-bbead6f40d.nii.gz /dcs05/ciprian/smart/ich_detection_challenge/data/nifti/ID-6002581a_ID-44c601efea.nii.gz /dcs05/ciprian/smart/ich_detection_challenge/data/nifti/ID-0011dd8c_ID-3d6598aed9.nii.gz /dcs05/ciprian/smart/ich_detection_challenge/data/nifti/ID-c048f060_ID-4da5642b38.nii.gz"
ifile=${files}
brain_outdir="data/brain_extracted_brainchop"
mkdir -p ${brain_outdir}
mask_outdir="data/brain_mask_brainchop"
mkdir -p ${mask_outdir}

for ifile in $files;
do
  echo "$ifile"
  bn=`basename ${ifile}`
  brain="${brain_outdir}/${bn}"
  mask="${mask_outdir}/${bn}"
  if [ ! -f "${brain}"  ] || [ ! -f "${mask}"  ]; then
    echo "processing ${ifile}"
    if [ -z "$CUDA_VISIBLE_DEVICES" ];
    then
      echo "using LLVM"
      LLVM=1 brainchop ${ifile} --output ${brain} --mask ${mask} --ct --model mindgrab
    else
      echo "using GPU"
      GPU=1 brainchop ${ifile} --output ${brain} --mask ${mask} --ct --model mindgrab
    fi
  fi
done

