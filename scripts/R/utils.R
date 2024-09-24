# utils.R

# Load required packages
source("scripts/R/constants.R")
require(stringr)
require(dplyr)
require(tidyr)
require(locits)
require(rstatix)
require(nlme)
require(lme4)
require(multcomp)
require(reshape2)
require(emmeans)
require(ggpubr)
require(ggplot2)

# Function to check and install missing packages using pak
install_if_missing <- function(packages) {
  # Ensure pak is installed
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }

  # Find packages that are not installed
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

  # Install missing packages using pak
  if (length(missing_packages)) {
    pak::pkg_install(missing_packages)
  }

  # Load the required packages
  invisible(lapply(packages, library, character.only = TRUE))
}

# Run the function to install and load packages
install_if_missing(REQUIRED_PACKAGES)

# Function to load master data and filter
load_master_data <- function(base_dir) {
  master_data_path <- file.path(base_dir, "LBAL_Master_Data_Sheet.csv")
  master_data <- read.csv(master_data_path)
  master_data <- master_data[!(master_data$Remove. == "Yes"), ]
  return(master_data)
}

create_video_df <- function(master_data, output_file) {
  # Initialize lists to store data
  contrast_list <- list() # Full list of all contrasts
  nest_ID_list <- list() # List of nest ID contrasts
  partner_list <- list() # To track IDs already processed
  video_df <- data.frame( # Empty dataframe to store video data
    videoname = character(),
    contrast = character(),
    nest_ID_contrast = character(),
    stringsAsFactors = FALSE
  )

  # Loop through each row of master_data
  for (i in seq_len(nrow(master_data))) {
    # Skip if specimen has already been processed
    if (master_data$Specimen.ID[i] %in% unlist(partner_list)) {
      next
    }

    # Find partner's specimen ID
    partner_i <- master_data$CT.Partner.ID[i]

    # Get caste information for specimen and partner
    caste_1 <- master_data$Caste[i]
    caste_2 <- master_data[master_data$Specimen.ID == partner_i, ]$Caste

    # Ensure both specimens have a caste
    if (length(caste_2) == 0) {
      next
    }

    # Determine caste contrast
    castes_1_and_2 <- c(caste_1, caste_2)
    castes_in_order <- castes_1_and_2[order(castes_1_and_2)] # Order castes alphabetically
    contrast <- paste(castes_in_order[1], castes_in_order[2], sep = "-")

    # Get nest letters for specimen and partner
    nest_ID_1 <- master_data$Nest.Letter[i]
    nest_ID_2 <- master_data[master_data$Specimen.ID == partner_i, ]$Nest.Letter

    # Ensure both specimens have a nest ID
    if (length(nest_ID_2) == 0) {
      next
    }

    # Determine nest ID contrast
    nest_IDs_1_and_2 <- c(nest_ID_1, nest_ID_2)
    nest_IDs_in_order <- nest_IDs_1_and_2[order(nest_IDs_1_and_2)] # Order nest IDs alphabetically
    nest_ID_contrast <- paste(nest_IDs_in_order[1], nest_IDs_in_order[2], sep = "-")

    # Store the contrasts and partner information
    contrast_list[[i]] <- contrast
    nest_ID_list[[i]] <- nest_ID_contrast
    partner_list[[i]] <- partner_i

    # Add the video information to the dataframe
    video_df[i, ] <- c(master_data$Videoname[i], contrast, nest_ID_contrast)
  }

  # Remove any incomplete rows (with NAs)
  video_df <- video_df[complete.cases(video_df), ]

  # Output the contrast table for sample size analysis
  contrast_table <- as.data.frame(table(unlist(contrast_list)))

  # Write the video_df to a CSV file
  write.csv(video_df, file = output_file, row.names = FALSE)

  # Return both the contrast table and video_df for further use
  return(list(video_df = video_df, contrast_table = contrast_table))
}

# Function to generate paths for vels.csv files based on video and contrast lists
generate_vels_file_paths <- function(video_list, contrast_list, base_dir_vels) {
  video_list_vels <- list()

  for (i in seq_along(video_list)) {
    video_list_vels[i] <- file.path(base_dir_vels, paste(video_list[i], contrast_list[i], "vels.csv", sep = "_"))
  }

  return(unlist(video_list_vels))
}

# Function to calculate motion proportion (proportion of time bees are moving >= 1 mm/s)
calculate_motion_proportion <- function(video_list_vels) {
  motion_prop_list <- list()

  for (i in seq_along(video_list_vels)) {
    if (file.exists(video_list_vels[i])) {
      vels <- read.csv(video_list_vels[i])

      # Exclude initial 200 frames
      vels <- vels[-(1:200), ]

      total_frames <- nrow(vels)
      moving_frames <- sum((vels[, 2] * 0.0946 * 20) >= 1, na.rm = TRUE)

      motion_prop_list[i] <- moving_frames / total_frames
    } else {
      motion_prop_list[i] <- NA
    }
  }

  return(unlist(motion_prop_list))
}

# Function to load bout counts (head-to-head and head-to-body interactions)
load_bout_counts <- function(video_list, contrast_list, base_dir_bouts) {
  hth_bout_list <- numeric()
  htb_bout_list <- numeric()

  for (i in seq_along(video_list)) {
    hth_filepath <- file.path(base_dir_bouts, paste(video_list[i], contrast_list[i], "hth_bouts.csv", sep = "_"))
    htb_filepath <- file.path(base_dir_bouts, paste(video_list[i], contrast_list[i], "htb_bouts.csv", sep = "_"))

    # Head-to-head bouts
    if (file.exists(hth_filepath)) {
      hth_df <- read.csv(hth_filepath)
      hth_bouts <- length(unique(hth_df$Bout))
    } else {
      hth_bouts <- 0
    }
    hth_bout_list <- c(hth_bout_list, hth_bouts)

    # Head-to-body bouts
    if (file.exists(htb_filepath)) {
      htb_df <- read.csv(htb_filepath)
      htb_bouts <- length(unique(htb_df$Bout))
    } else {
      htb_bouts <- 0
    }
    htb_bout_list <- c(htb_bout_list, htb_bouts)
  }

  return(list(hth_bouts = hth_bout_list, htb_bouts = htb_bout_list))
}

# Function to prepare data for linear mixed model
prepare_lmm_data <- function(motion_prop_list, hth_bout_list, htb_bout_list, contrast_list, video_list, nest_site_ids) {
  lmm_df <- data.frame(
    motion_prop = motion_prop_list,
    hth_bouts = hth_bout_list,
    htb_bouts = htb_bout_list,
    contrast = factor(contrast_list, levels = unique(contrast_list)),
    videoname = video_list,
    nest_site_id = nest_site_ids
  )

  lmm_df$date <- sapply(lmm_df$videoname, function(x) strsplit(x, "_")[[1]][1])

  return(lmm_df)
}

# Function to save plots
save_plot <- function(filename, plot_object, width = 8, height = 6) {
  ggsave(filename, plot = plot_object, width = width, height = height)
}
