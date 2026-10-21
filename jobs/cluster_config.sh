# Settings shared by jobs/send2cluster.sh (workstation), jobs/setup_cluster.sh (login node) and
# jobs/_task.sh (compute node). Sourced, not run.

# Copy of the project on the shared storage: compute nodes cannot see the workstation's disk
CLUSTER_DIR=/network/iss/cenir/analyse/meeg/00_max/SEA-SIG-OHBM/CarbonFootprint
LOGIN_HOST=sphpc-login02

# R 4.5.0 from the shared Spack tree (same version as renv.lock): its path resolves identically on
# the login and compute nodes, unlike the workstation's R and package library
RSCRIPT=/network/iss/apps/software/scit/spack_installs/linux-rocky8-broadwell/gcc-8.5.0/r-4.5.0-rxslzhbrf2rizgrrowx5nxn3euorpd2h/bin/Rscript

# renv on the cluster: no cache in $HOME, so the library lives entirely in CLUSTER_DIR/renv/library;
# and no startup warning about the renv.lock packages that are not installed there (only those
# that the cluster scripts need are, see jobs/setup_cluster.R)
export RENV_CONFIG_CACHE_ENABLED=FALSE
export RENV_CONFIG_SYNCHRONIZED_CHECK=FALSE
