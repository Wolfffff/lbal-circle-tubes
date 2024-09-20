# constants.R

# Required packages
required_packages <- c("ggplot2", "nlme", "multcomp", "stringr", "dplyr", "tidyr")

# Color settings for plots
plot_colors <- c(
  "queens" = "#501617",
  "workers" = "#F38AB3",
  "solitary_reproductives" = "#355C67",
  "proportion of time spent moving (%)" = "#f7a278",
  "total head-to-head interactions" = "#be97c6",
  "total head-to-body interactions" = "#2e294e"
)

# Base directories for file reading
base_dir_master_data <- "data"
base_dir_velocities <- "data/processed_data/velocities"
base_dir_batch_degree <- "data/processed_data/batch_degree"
