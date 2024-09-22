# utils.R

source("scripts/R/constants.R")

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
install_if_missing(required_packages)

# Load master data and filter
load_master_data <- function(base_dir) {
  master_data_path <- file.path(base_dir, "LBAL_Master_Data_Sheet.csv")
  master_data <- read.csv(master_data_path)
  master_data <- master_data[!(master_data$Remove. == "Yes"), ]
  return(master_data)
}

# Preprocess data into dataframes for each caste
prepare_caste_data <- function(master_data) {
  queen_df <- as.numeric(master_data$Ovarian.Index[master_data$Caste == "queen"])
  worker_df <- as.numeric(master_data$Ovarian.Index[master_data$Caste == "worker"])
  solitary_df <- as.numeric(master_data$Ovarian.Index[master_data$Caste == "solitary"])

  max_length <- max(length(queen_df), length(worker_df), length(solitary_df))

  queen_df <- c(queen_df, rep(NA, max_length - length(queen_df)))
  worker_df <- c(worker_df, rep(NA, max_length - length(worker_df)))
  solitary_df <- c(solitary_df, rep(NA, max_length - length(solitary_df)))

  plot_df <- data.frame(queens = queen_df, workers = worker_df, solitary_reproductives = solitary_df)
  return(plot_df)
}

# Prepare motion, head-to-head, and head-to-body interaction data from velocity files
load_motion_data <- function(video_list_vels) {
  motion_prop_list <- list()

  for (i in seq_along(video_list_vels)) {
    vels <- read.csv(video_list_vels[i])
    frame_count <- 0
    for (j in 200:nrow(vels)) {
      if (is.na(vels[j, 2])) next
      if ((vels[j, 2] * 0.0946 * 20) < 1) {
        frame_count <- frame_count + 1
      }
    }
    motion_prop_list[i] <- frame_count / (nrow(vels) - 200)
  }
  motion_prop_list <- unlist(motion_prop_list)
  return(motion_prop_list)
}

# Function to load head-to-head and head-to-body bout data
load_bout_data <- function(video_list_hth, video_list_htb) {
  hth_bout_list <- list()
  htb_bout_list <- list()

  for (i in seq_along(video_list_hth)) {
    hth_bout_list[i] <- if (file.exists(video_list_hth[i])) length(unique(read.csv(video_list_hth[i])$Bout)) else 0
    htb_bout_list[i] <- if (file.exists(video_list_htb[i])) length(unique(read.csv(video_list_htb[i])$Bout)) else 0
  }

  return(list(hth_bout_list = unlist(hth_bout_list), htb_bout_list = unlist(htb_bout_list)))
}

# Prepare the linear mixed model dataframe
prepare_lmm_data <- function(motion_prop_list, hth_bout_list, htb_bout_list, contrast_list, video_list) {
  lmm_df <- data.frame(
    motion_prop = motion_prop_list,
    hth_bouts = hth_bout_list,
    htb_bouts = htb_bout_list,
    contrast = factor(contrast_list, levels = c("queen_solitary", "solitary_solitary", "worker_worker", "queen_worker", "queen_queen")),
    videoname = video_list
  )

  lmm_df$date <- sapply(lmm_df$videoname.videoname, function(x) strsplit(x, "_")[[1]][1])
  return(lmm_df)
}

generate_video_list_and_contrast <- function(base_dir, file_patterns) {
  video_list <- list()
  contrast_list <- list()

  for (pattern in names(file_patterns)) {
    file_list <- list.files(path = base_dir, pattern = file_patterns[[pattern]], full.names = TRUE)

    if (length(file_list) > 0) {
      video_list <- append(video_list, file_list)
      contrast_list <- append(contrast_list, rep(pattern, length(file_list)))
    }
  }

  return(list(video_list = unlist(video_list), contrast_list = unlist(contrast_list)))
}


# Function to load video lists from files and create a corresponding contrast list
load_video_list_and_contrast <- function(base_dir) {
  video_list_que_que <- read.delim(file.path(base_dir, "list_of_files_queen_queen.txt"), header = FALSE)
  video_list_que_wor <- read.delim(file.path(base_dir, "list_of_files_queen_worker.txt"), header = FALSE)
  video_list_wor_wor <- read.delim(file.path(base_dir, "list_of_files_worker_worker.txt"), header = FALSE)
  video_list_que_sol <- read.delim(file.path(base_dir, "list_of_files_queen_solitary.txt"), header = FALSE)
  video_list_sol_sol <- read.delim(file.path(base_dir, "list_of_files_solitary_solitary.txt"), header = FALSE)

  video_list <- unlist(c(video_list_que_que, video_list_que_wor, video_list_wor_wor, video_list_que_sol, video_list_sol_sol))
  contrast_list <- c(
    rep("queen_queen", nrow(video_list_que_que)),
    rep("queen_worker", nrow(video_list_que_wor)),
    rep("worker_worker", nrow(video_list_wor_wor)),
    rep("queen_solitary", nrow(video_list_que_sol)),
    rep("solitary_solitary", nrow(video_list_sol_sol))
  )

  return(list(video_list = video_list, contrast_list = contrast_list))
}

# Function to generate paths for vels.csv files based on video and contrast lists
generate_vels_file_paths <- function(video_list, contrast_list, base_dir_vels) {
  video_list_vels <- list()

  for (i in seq_along(video_list)) {
    video_list_vels[i] <- file.path(base_dir_vels, paste0(video_list[i], "_", contrast_list[i], "_vels.csv"))
  }

  return(unlist(video_list_vels))
}

# Function to calculate motion proportion (proportion of time bees are moving < 1 mm/s)
calculate_motion_proportion <- function(video_list_vels) {
  motion_prop_list <- list()

  for (i in seq_along(video_list_vels)) {
    vels <- read.csv(video_list_vels[i])

    frame_count <- 0
    for (j in 200:nrow(vels)) {
      if (!is.na(vels[j, 2]) && (vels[j, 2] * 0.0946 * 20) < 1) {
        frame_count <- frame_count + 1
      }
    }

    motion_prop_list[[i]] <- frame_count / (nrow(vels) - 200)
  }

  return(unlist(motion_prop_list))
}

# Function to create contrast and nest_ID_contrast from master data
create_video_df <- function(master_data) {
  contrast_list <- list() # full list of all contrasts
  nest_ID_list <- list()
  partner_list <- list() # keep track of IDs you've already accounted for
  video_df <- data.frame(
    videoname = character(),
    contrast = character(),
    nest_ID_contrast = character()
  )

  for (i in seq_len(nrow(master_data))) {
    # Skip if this specimen is already accounted for
    if (master_data$Specimen.ID[i] %in% unlist(partner_list)) {
      next
    }

    # Get partner info
    partner_i <- master_data$CT.Partner.ID[i]

    # Determine caste contrast
    caste_1 <- master_data$Caste[i]
    caste_2 <- master_data[master_data$Specimen.ID == partner_i, ]$Caste
    castes_1_and_2 <- c(caste_1, caste_2)
    castes_in_order <- castes_1_and_2[str_order(castes_1_and_2)]
    contrast <- paste(castes_in_order[1], castes_in_order[2], sep = "-")

    # Determine nest_ID contrast
    nest_ID_1 <- master_data$Nest.Letter[i]
    nest_ID_2 <- master_data[master_data$Specimen.ID == partner_i, ]$Nest.Letter
    nest_IDs_1_and_2 <- c(nest_ID_1, nest_ID_2)
    nest_IDs_in_order <- nest_IDs_1_and_2[str_order(nest_IDs_1_and_2)]
    nest_ID_contrast <- paste(nest_IDs_in_order[1], nest_IDs_in_order[2], sep = "-")

    # Store results
    contrast_list[i] <- contrast
    nest_ID_list[i] <- nest_ID_contrast
    partner_list[i] <- partner_i

    video_df[i, ] <- c(master_data$Videoname[i], contrast, nest_ID_contrast)
  }

  # Filter out incomplete rows
  video_df <- video_df[complete.cases(video_df), ]

  return(video_df)
}
