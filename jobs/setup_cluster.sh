#!/bin/bash
# Builds the cluster copy's R package library (jobs/setup_cluster.R). Run on the login node in a
# login shell, for `module`; jobs/send2cluster.sh setup does that over ssh. Once, and again after
# renv.lock changes.
set -euo pipefail
cd "$(dirname "$0")/.."
source jobs/cluster_config.sh

module load proxy # internet, login node only
"$RSCRIPT" jobs/setup_cluster.R
