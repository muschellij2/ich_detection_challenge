module load conda;
source activate /jhpce/shared/jhpce/core/conda/miniconda3-4.6.14/envs/tensorflow-gpu;
nvidia-smi;
which nvidia-smi ;
export CUDA_VISIBLE_DEVICES=0;
echo "CUDA_VISIBLE_DEVICES is ${CUDA_VISIBLE_DEVICES}"
export HDF5_USE_FILE_LOCKING=FALSE ;
R --no-save < cnn_128.R
