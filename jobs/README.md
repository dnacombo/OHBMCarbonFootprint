# Running scripts on the ICM `sphpc` cluster

Slow scripts (`13b_`, several hours, and `13_`) can run on the SLURM cluster instead of the workstation.
Everything goes through `jobs/send2cluster.sh`, run from the project root on the workstation. Passwordless ssh
to `sphpc-login02` must work (key or agent).

The project lives on the workstation's own disk, which compute nodes cannot see. So the script copies it to
`/network/iss/cenir/analyse/meeg/00_max/SEA-SIG-OHBM/CarbonFootprint` (`CLUSTER_DIR` in
`jobs/cluster_config.sh`), the job runs there, and `fetch` copies the outputs back.

## Once: build the R library on the cluster

```bash
bash jobs/send2cluster.sh setup
```

This syncs the project, then on the login node (internet through `module load proxy`) installs into the
cluster copy's `renv/library` the packages `13_`/`13b_` need, at their `renv.lock` versions. It uses the shared
Spack R 4.5.0, which is the same version as `renv.lock`. Binaries come from Posit Package Manager (RHEL 8), so
there is little compiling. It ends by checking that the packages load and that a PNG can be written. Re-run it
after `renv.lock` changes. Other scripts may need more packages: add them to `packages` in
`jobs/setup_cluster.R` first. `sf`/`terra` need GDAL/GEOS/PROJ, which the cluster may lack.

The workstation's library can't be reused: it was built for a different system.

## Submit, follow, fetch

```bash
bash jobs/send2cluster.sh R/13b_equitable_hub_selection_pop250k.R
bash jobs/send2cluster.sh --time 72:00:00 --mem 32G R/13b_equitable_hub_selection_pop250k.R   # overrides
bash jobs/send2cluster.sh --dry-run R/13b_equitable_hub_selection_pop250k.R                   # show, run nothing

bash jobs/send2cluster.sh status          # squeue -u $USER
bash jobs/send2cluster.sh fetch           # copy new outputs back into output/

# chaining (afterok):
id=$(bash jobs/send2cluster.sh --parsable R/13_equitable_hub_selection.R)
bash jobs/send2cluster.sh --after "$id" R/some_script.R
```

Each submission first syncs the project, so the job runs the code as it is on the workstation now, committed or
not. `jobs/_task.sh` is the wrapper run on the node. Its `#SBATCH` defaults are partition `compute`, 4 CPUs,
16 GB and 48 h; `--time`, `--mem`, `--cpus` and `--partition` override them.

Log: `CLUSTER_DIR/jobs/logs/<job name>_<job id>.out`. The default job name is `co2-<script name>`. To see the
memory and time actually used: `ssh sphpc-login02 sacct -j <id> --format=JobID,MaxRSS,Elapsed,State`.

## Notes

- Copies never overwrite a newer file (`rsync --update`) and never delete. So re-syncing code doesn't clobber
  results you haven't fetched, and fetching doesn't clobber newer local files. A file deleted on one side stays
  on the other.
- Before resubmitting a script, fetch the results of the previous run. Otherwise its outputs on the cluster are
  newer than yours, so the sync keeps them, and the job overwrites them anyway.
- `run_all.R` also works on the cluster (`--cpus 8`). It still needs network for new geocoding and every
  package in `renv.lock`, so the cluster library would have to include them all.
