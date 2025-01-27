# constants.R

require(ggplot2)

# Required packages
REQUIRED_PACKAGES <- c(
  "ggplot2",
  "nlme",
  "multcomp",
  "stringr",
  "dplyr",
  "tidyr",
  "locits",
  "rstatix",
  "lme4",
  "reshape2",
  "emmeans",
  "ggpubr"
)

# Base directories for file reading
BASE_DIR_MASTER_DATA <- "../../data"
BASE_DIR_PROCESSED_DATA <- "../../data/processed_data_BORIS"
BASE_DIR_VELOCITIES <- file.path(BASE_DIR_PROCESSED_DATA, "velocities")
BASE_DIR_BATCH_DEGREE <- file.path(BASE_DIR_PROCESSED_DATA, "batch_degree")

# Color settings for plots... ugly for now.
CONTRAST_COLORS <- c(
  "queen_solitary" = "#E1BE6A",
  "queen_queen" = "#40B0A6",
  "solitary_solitary" = "#4B644E",
  "queen_worker" = "#7C3463",
  "worker_worker" = "#9A4A4D"
)

CASTE_COLORS <- c(
  "queen" = "#501617",
  "worker" = "#F38AB3",
  "solitary" = "#355C67"
)

SHARED_THEME <- theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.background = element_rect(fill = "white"),
    legend.position = "none"
  )
