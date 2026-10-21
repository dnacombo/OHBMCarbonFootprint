# run_all.R
# Runs the whole pipeline (R/01_ ... R/13b_), each script in its own Rscript process, running
# scripts in parallel as soon as the scripts they depend on have finished.
#
# Usage, from the project root:
#   Rscript run_all.R            # all cores but one
#   Rscript run_all.R --jobs 4   # at most 4 scripts at a time (--jobs 1 = sequential)
#
# Parallel runs need fork (Linux/macOS); elsewhere scripts run one after another.
# Each script's console output goes to output/logs/<script>.log. If a script fails, the
# scripts that depend on it are skipped, the others still run, and the exit status is 1.
# Needs network access for geocoding new addresses (03_, and hubs not yet in the cache) and,
# if data/openalex_neuroimaging_institutions.csv is missing, OPENALEX_API_KEY for 11_.

# --- 1. Dependencies: script -> scripts whose outputs it reads ---
pipeline <- list(
  "01_data_loading"                  = character(0),
  "02_collect_synonyms"              = "01_data_loading",
  "03_geocode"                       = "02_collect_synonyms",
  "04_all_attendees_maps"            = "03_geocode",
  "05_monocentric_footprint"         = "03_geocode",
  "05b_monocentric_footprint_train"  = "03_geocode",
  "06_multicentric_footprint"        = "05_monocentric_footprint",
  "06b_multicentric_footprint_train" = "05b_monocentric_footprint_train",
  "07_multicentric_benchmark"        = "03_geocode",
  "08_attraction_power"              = "03_geocode",
  "09_interactive_map"               = "05_monocentric_footprint",
  "10_greedy_hub_selection"          = "03_geocode",
  "11_openalex_community"            = "03_geocode",
  "12_participation_model"           = "11_openalex_community",
  "13_equitable_hub_selection"       = c("10_greedy_hub_selection", "11_openalex_community",
                                         "12_participation_model"),
  "13b_equitable_hub_selection_pop250k" = c("10_greedy_hub_selection", "11_openalex_community",
                                            "12_participation_model")
)
# 13b_ (several hours) and 13_ (~1 h) are by far the slowest: start the scripts leading to them first
critical_path <- c("10_greedy_hub_selection", "11_openalex_community", "12_participation_model")

# --- 2. Settings ---
if (!file.exists("R/utils.R")) stop("Run this from the project root: Rscript run_all.R")
missing_scripts <- names(pipeline)[!file.exists(file.path("R", paste0(names(pipeline), ".R")))]
if (length(missing_scripts) > 0) stop("Missing scripts: ", paste(missing_scripts, collapse = ", "))

args <- commandArgs(trailingOnly = TRUE)
use_fork <- .Platform$OS.type == "unix"
n_jobs <- if (use_fork) max(1, parallel::detectCores() - 1, na.rm = TRUE) else 1
if ("--jobs" %in% args) n_jobs <- as.integer(args[which(args == "--jobs") + 1])
if (is.na(n_jobs) || n_jobs < 1) stop("--jobs must be a positive integer")
if (!use_fork && n_jobs > 1) {
  message("Parallel runs need fork (Linux/macOS): running sequentially")
  n_jobs <- 1
}

log_dir <- "output/logs"
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")

# Runs one script in a fresh R session; returns its exit status (0 = success)
run_script <- function(name) {
  log_file <- file.path(log_dir, paste0(name, ".log"))
  system2(rscript, file.path("R", paste0(name, ".R")), stdout = log_file, stderr = log_file)
}

stamp <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

# --- 3. Scheduler ---
status <- setNames(rep("pending", length(pipeline)), names(pipeline)) # -> running/done/failed/skipped
started <- setNames(rep(Sys.time(), length(pipeline)), names(pipeline))
elapsed <- setNames(rep(NA_real_, length(pipeline)), names(pipeline))
running <- list() # pid (as character) -> script name

finish <- function(name, exit_code) {
  elapsed[[name]] <<- as.numeric(difftime(Sys.time(), started[[name]], units = "mins"))
  ok <- identical(as.integer(exit_code), 0L)
  status[[name]] <<- if (ok) "done" else "failed"
  stamp(if (ok) "done   " else "FAILED ", name, sprintf("(%.1f min)", elapsed[[name]]),
        if (!ok) paste("- see", file.path(log_dir, paste0(name, ".log"))))
}

stamp(sprintf("Running %d scripts, up to %d at a time; logs in %s/", length(pipeline), n_jobs, log_dir))
t0 <- Sys.time()
while (any(status %in% c("pending", "running"))) {
  pending <- names(status)[status == "pending"]

  # skip scripts whose inputs will never be produced
  for (name in pending) {
    if (any(status[pipeline[[name]]] %in% c("failed", "skipped"))) {
      status[[name]] <- "skipped"
      stamp("skipped", name, "(a script it depends on failed)")
    }
  }

  ready <- names(status)[status == "pending" &
                           vapply(pipeline, \(d) all(status[d] == "done"), logical(1))]
  ready <- ready[order(!ready %in% critical_path)]

  for (name in head(ready, n_jobs - length(running))) {
    stamp("start  ", name)
    started[[name]] <- Sys.time()
    if (n_jobs == 1) {
      finish(name, run_script(name))
    } else {
      status[[name]] <- "running"
      job <- parallel::mcparallel(run_script(name), silent = TRUE)
      running[[as.character(job$pid)]] <- name
    }
  }

  if (length(running) > 0) {
    results <- parallel::mccollect(as.integer(names(running)), wait = FALSE, timeout = 5)
    for (pid in names(results)) {
      # a child that crashed returns a try-error instead of the exit status
      exit_code <- if (is.numeric(results[[pid]])) results[[pid]] else 1L
      finish(running[[pid]], exit_code)
      running[[pid]] <- NULL
    }
  } else if (!any(status == "pending")) {
    break
  }
}

# --- 4. Summary ---
cat("\n")
print(data.frame(script = names(status), status = unname(status),
                 minutes = round(unname(elapsed), 1)), row.names = FALSE)
stamp(sprintf("Total: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
if (any(status != "done")) quit(status = 1)
