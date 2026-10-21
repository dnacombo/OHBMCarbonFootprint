#!/bin/bash
# Runs project R scripts on the sphpc SLURM cluster. From the workstation, in the project root:
#
#   bash jobs/send2cluster.sh setup                 # once (and after renv.lock changes): build the R library
#   bash jobs/send2cluster.sh [options] <R script>  # sync the project, submit the script
#   bash jobs/send2cluster.sh fetch                 # copy new outputs back into output/
#   bash jobs/send2cluster.sh status                # squeue
#
# e.g.  bash jobs/send2cluster.sh R/13b_equitable_hub_selection_pop250k.R
#       bash jobs/send2cluster.sh --time 72:00:00 --mem 32G R/13b_equitable_hub_selection_pop250k.R
#
# Options (before the script):
#   --time T, --mem M, --cpus N, --partition P   override the #SBATCH defaults of jobs/_task.sh
#                                                (compute, 4 CPUs, 16G, 48:00:00)
#   --name N       job name (default co2-<script name>)
#   --after ID     start after job ID succeeded (--dependency=afterok:ID)
#   --parsable     print only the job ID (for chaining)
#   --dry-run      print the sync and sbatch commands, run nothing
#
# The project lives on the workstation's own disk, which compute nodes cannot see: it is copied to
# CLUSTER_DIR (jobs/cluster_config.sh) on the shared storage, the job runs there, and `fetch` copies
# the outputs back. Copies never overwrite a newer file (rsync --update), so syncing code does not
# clobber results not yet fetched, and fetching does not clobber newer local files.
# sbatch runs on the login node over ssh (passwordless ssh to LOGIN_HOST needed), or locally if
# sbatch is available.
set -euo pipefail
PROJECT="$(cd "$(dirname "$0")/.." && pwd)"
source "$PROJECT/jobs/cluster_config.sh"

die() { echo "send2cluster: $*" >&2; exit 1; }
DRY=false

run() { # run a command, or only print it with --dry-run
  if $DRY; then printf '#'; printf ' %q' "$@"; echo; else "$@"; fi
}

on_login() { # run a shell command line on the login node (in a login shell, for `module`)
  if command -v sbatch >/dev/null; then run bash -lc "$1"
  else run ssh -o ConnectTimeout=10 "$LOGIN_HOST" "bash -lc $(printf %q "$1")"; fi
}

sync_to_cluster() {
  [[ "$PROJECT" == "$CLUSTER_DIR" ]] && return # already running from the cluster copy
  $DRY || mkdir -p "$CLUSTER_DIR/jobs/logs"
  # code, data and current outputs; not the workstation's R library (built for another system),
  # git history, editor/session dotfiles, archives or logs. Excluded paths are never deleted.
  run rsync -a --update \
    --include=/.Rprofile --exclude='/.*' \
    --exclude=/renv/library/ --exclude=/renv/staging/ \
    --exclude=/output/archive/ --exclude=/output/logs/ --exclude=/jobs/logs/ \
    "$PROJECT/" "$CLUSTER_DIR/"
}

# --- parse options ---
sbatch_opts=()
name=""
parsable=false
while [[ $# -gt 0 && "$1" == --* ]]; do
  case "$1" in
    --time|--mem|--partition) sbatch_opts+=("$1=${2:?$1 needs a value}"); shift 2 ;;
    --cpus) sbatch_opts+=("--cpus-per-task=${2:?--cpus needs a value}"); shift 2 ;;
    --name) name="${2:?--name needs a value}"; shift 2 ;;
    --after) sbatch_opts+=("--dependency=afterok:${2:?--after needs a job ID}"); shift 2 ;;
    --parsable) parsable=true; shift ;;
    --dry-run) DRY=true; shift ;;
    *) die "unknown option $1 (see the header of jobs/send2cluster.sh)" ;;
  esac
done
[[ $# -ge 1 ]] || die "usage: bash jobs/send2cluster.sh setup | fetch | status | [options] <R script>"

case "$1" in
  setup)
    sync_to_cluster
    on_login "cd $(printf %q "$CLUSTER_DIR") && bash jobs/setup_cluster.sh"
    ;;
  fetch)
    run rsync -a --update "$CLUSTER_DIR/output/" "$PROJECT/output/"
    $DRY || echo "fetched new outputs from $CLUSTER_DIR/output/ (logs: $CLUSTER_DIR/jobs/logs/)"
    ;;
  status)
    on_login 'squeue -u $USER'
    ;;
  *)
    script="$1"
    [[ -f "$PROJECT/$script" && "$script" == *.R ]] || die "no R script $script in $PROJECT"
    [[ -z "$name" ]] && name="co2-$(basename "$script" .R)"
    $DRY || [[ -d "$CLUSTER_DIR/renv/library" ]] || \
      echo "warning: no R library in the cluster copy yet: run 'bash jobs/send2cluster.sh setup' first" >&2
    sync_to_cluster
    cmd="cd $(printf %q "$CLUSTER_DIR") && sbatch --parsable --job-name=$(printf %q "$name")"
    cmd+=" --output=$(printf %q "$CLUSTER_DIR/jobs/logs/%x_%j.out")"
    for o in "${sbatch_opts[@]}"; do cmd+=" $(printf %q "$o")"; done
    cmd+=" jobs/_task.sh $(printf %q "$script")"
    if $DRY; then on_login "$cmd"; exit 0; fi
    # last line only: a chatty .bashrc on the login node must not spoil the ID
    out="$(on_login "$cmd")" || die "submission failed (ssh to $LOGIN_HOST or sbatch): see the message above"
    job_id="$(tail -n 1 <<< "$out" | cut -d';' -f1)"
    [[ "$job_id" =~ ^[0-9]+$ ]] || die "unexpected sbatch output: $out"
    if $parsable; then echo "$job_id"; else
      echo "submitted: $name  job $job_id"
      echo "log:       $CLUSTER_DIR/jobs/logs/${name}_${job_id}.out"
      echo "follow:    bash jobs/send2cluster.sh status  |  ssh $LOGIN_HOST sacct -j $job_id --format=JobID,MaxRSS,Elapsed,State"
      echo "when done: bash jobs/send2cluster.sh fetch"
    fi
    ;;
esac
