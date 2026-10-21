#!/bin/bash
# SLURM wrapper run on the compute node: runs one R script of the project with the cluster's R.
# Do not sbatch it directly: jobs/send2cluster.sh syncs the project, then submits it from
# CLUSTER_DIR with the log path and any resource overrides.
#   $1  script, relative to the project root (e.g. R/13b_equitable_hub_selection_pop250k.R)
#
#SBATCH --partition=compute
#SBATCH --cpus-per-task=4
#SBATCH --mem=16G
#SBATCH --time=48:00:00

set -euo pipefail
# sbatch runs a copy of this file from the spool directory: use the submission directory
cd "${SLURM_SUBMIT_DIR:?submit through jobs/send2cluster.sh}"
source jobs/cluster_config.sh

SCRIPT="${1:?usage: _task.sh <R script> - submit through jobs/send2cluster.sh}"
test -d renv/library || { echo "no R library in $PWD/renv/library: run 'bash jobs/send2cluster.sh setup' first"; exit 1; }

# R itself is single-threaded here; let BLAS use the CPUs of the job
export OMP_NUM_THREADS="${SLURM_CPUS_PER_TASK:-1}"
export OPENBLAS_NUM_THREADS="${SLURM_CPUS_PER_TASK:-1}"

echo "host=$(hostname) job=${SLURM_JOB_NAME:-?} id=${SLURM_JOB_ID:-?} $(date)"
echo "cmd: $RSCRIPT $SCRIPT"
srun "$RSCRIPT" "$SCRIPT"
echo "done $(date)"
