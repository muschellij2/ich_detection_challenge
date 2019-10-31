module load conda;
source activate /jhpce/shared/jhpce/core/conda/miniconda3-4.6.14/envs/tensorflow-gpu;
nvidia-smi;
export CUDA_VISIBLE_DEVICES=1;
R --no-save < cnn_128.R
