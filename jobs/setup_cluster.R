# jobs/setup_cluster.R
# Installs, with the cluster's R, the packages that 13_/13b_ need into the renv library of the
# cluster copy of the project, at the versions in renv.lock (and their dependencies). Run on the
# login node (internet through the proxy) by jobs/setup_cluster.sh, not directly.
# Other scripts need more packages (sf, terra, ...: system libraries the cluster may not have):
# add them to `packages` before sending those scripts to the cluster.

packages <- c("tidyverse", "geosphere", "countrycode", "maps", "knitr", "scales")

# Posit Package Manager has binaries for RHEL/Rocky 8: most packages need no compilation
options(renv.config.repos.override = "https://packagemanager.posit.co/cran/__linux__/rhel8/latest")
renv::restore(packages = packages, prompt = FALSE)

# check that everything loads and that PNGs can be written without a display, as the jobs will
for (p in packages) suppressPackageStartupMessages(library(p, character.only = TRUE))
png_file <- tempfile(fileext = ".png")
ggsave(png_file, ggplot(data.frame(x = 1:3), aes(x, x)) + geom_point(), width = 2, height = 2)
stopifnot(file.exists(png_file))
cat("Cluster library ready:", .libPaths()[1], "\n")
