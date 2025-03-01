# utils.R

# Load required packages
source("constants.R")
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
require(patchwork)

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
  master_data_path <- file.path(base_dir, "LBAL_Master_Data_Sheet_BORIS.csv")
  master_data <- read.csv(master_data_path)
  master_data <- master_data[!(master_data$Remove. == "Yes"), ]
  return(master_data)
}

load_behavior_data <- function(base_dir) {
  behavior_data_path <- file.path(base_dir, "all_LBAL_events.csv")
  behavior_data <- read.csv(behavior_data_path)

  # Remove unnecessary columns
  behavior_data <- behavior_data[, c("Subject", "Behavior", "Behavioral.category", "Behavior.type", "Time", "Media.file.name", "Image.index")]

  # Extract videoname from Media.file.name
  behavior_data$videoname <- str_extract(behavior_data$Media.file.name, "[^/\\\\]+$")

  # Remove ".mp4" from videoname
  behavior_data$videoname <- str_remove(behavior_data$videoname, ".mp4")

  # Remove Media.file.name column now that we have the videoname
  behavior_data <- behavior_data[, !(names(behavior_data) %in% "Media.file.name")]

  return(behavior_data)
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

choose_plot_type <- function(plot_type) {

  if (plot_type == "regular_categories") {

    measures <- c("Aggressive", "Avoidant", "Neutral", "Cooperative", "All")
    metrics <- c("count", "avg_duration", "tot_duration")
    level = "category"

  } else if (plot_type == "proportion_categories") {

    measures <- c("Aggressive", "Avoidant", "Neutral", "Cooperative")
    metrics <- c("count_prop")
    level = "category"

  } else if (plot_type == "singles_aggr") {

    measures <- c("cposture", "lunge", "nudge", "bite")
    metrics <- c("count", "avg_duration", "tot_duration", "count_prop")
    level = "single"

  } else if (plot_type == "singles_avoi") {

    measures <- c("withdraw", "uturn", "back")
    metrics <- c("count", "avg_duration", "tot_duration", "count_prop")
    level = "single"

  } else if (plot_type == "singles_neut") {

    measures <- c("headtobody", "antennation", "tandemwalking")
    metrics <- c("count", "avg_duration", "tot_duration", "count_prop")
    level = "single"

  } else if (plot_type == "singles_coop") {

    measures <- c("pass", "headtohead", "sidebyside", "attemptedpass")
    metrics <- c("count", "avg_duration", "tot_duration", "count_prop")
    level = "single"

  } else {

    stop("Invalid plot type. Please choose from 'regular_categories', 'proportion_categories', 'singles_aggr', 'singles_avoi', 'singles_neut', or 'singles_coop'.")

  }

  return(list(measures = measures, metrics = metrics, level = level))
}

prop_data_pipeline <- function(behavior_data) {                                                                                   
  # 1. Extract unique subjects and map Caste based on Specimen.ID, Tag_Color, and videoname
  unique_subjects <- master_data %>%
    select(Videoname, Specimen.ID, Tag.Color, Caste) %>%
    distinct(Videoname, Specimen.ID, Tag.Color, Caste) %>%
    rename(videoname = Videoname, Tag_Color = Tag.Color)

  # 3. Merge behavior data and fill Caste based on unique_subjects
  count_prop_individual_data <- behavior_data %>%
    filter(Behavior.type == "START") %>%  # Only consider START times for counting
    mutate(Tag_Color = case_when(
      Subject == "Black Tag" ~ "B",
      Subject == "Purple Tag" ~ "P",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(Tag_Color)) %>%  # Remove undirected interactions
    left_join(master_data, by = c("videoname" = "Videoname", "Tag_Color" = "Tag.Color")) %>%
    select(Behavioral.category, Specimen.ID, Caste) %>%
    group_by(Specimen.ID, Behavioral.category, Caste) %>%
    summarise(count = n(), .groups = "drop") %>%
    complete(Specimen.ID = unique(master_data$Specimen.ID), 
            Behavioral.category = unique(behavior_data$Behavioral.category),
            fill = list(count = 0)) %>%

    # Join with unique_subjects to get the correct Caste values
    left_join(unique_subjects, by = c("Specimen.ID")) %>%

    # Now, fill the Caste based on the join, ensuring the correct Caste is mapped
    mutate(Caste = coalesce(Caste.y, Caste.x)) %>%  # Prefer Caste from the join over initial values
    select(-Caste.x, -Caste.y) %>%  # Remove duplicate columns

    # Calculate count_prop
    group_by(Specimen.ID) %>%
    mutate(count_prop_caste = ifelse(sum(count) == 0, 0, count / sum(count))) %>%
    
    # Adjust Behavioral.category values
    mutate(Behavioral.category = case_when(
      Behavioral.category == "Tolerant/Cooperative" ~ "Cooperative",
      TRUE ~ Behavioral.category
    )) %>%
    select(-count, -Tag_Color) %>%
    ungroup()

  return(count_prop_individual_data)

}

contrast_data_pipeline <- function(behavior_data, level) {
  # if level is "category", then we are looking at behavioral categories
  if (level == "category") {
    # Extract counts and durations for each social contrast
    count_data <- behavior_data %>%
      group_by(videoname, Behavioral.category) %>%
      filter(Behavior.type == "START") %>% # Only consider START times for counting
      dplyr::summarise(count = n(), .groups = "drop") %>%
      complete(videoname, Behavioral.category, fill = list(count = 0))

    count_prop_data <- count_data %>%
      group_by(videoname) %>%
      mutate(count_prop = count / sum(count)) %>%
      ungroup() %>%
      complete(videoname, Behavioral.category, fill = list(count_prop = 0))

    # Add a behavioral category for all interactions summing counts by videoname
    count_data_all <- count_data %>%
      group_by(videoname) %>%
      dplyr::summarise(count = sum(count), .groups = "drop") %>%
      left_join(count_data %>%
                  select(videoname, Behavioral.category) %>%
                  distinct() %>%
                  group_by(videoname) %>%
                  slice_head(n = 1),
                by = "videoname") %>%
      mutate(Behavioral.category = "All")

    count_data <- bind_rows(count_data, count_data_all)

    avg_duration_data <- behavior_data %>%
      arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
      mutate(
        next_time = lead(Time),
        Duration = next_time - Time
      ) %>%
      filter(Behavior.type == "START") %>%
      group_by(videoname, Behavioral.category) %>%
      dplyr::summarise(avg_duration = mean(Duration), .groups = "drop") %>%
      complete(videoname, Behavioral.category, fill = list(avg_duration = 0))

    # Add a behavioral category for all interactions summing counts by videoname
    avg_duration_data_all <- avg_duration_data %>%
      group_by(videoname) %>%
      dplyr::summarise(avg_duration = mean(avg_duration), .groups = "drop") %>%
      left_join(avg_duration_data %>%
                  select(videoname, Behavioral.category) %>%
                  distinct() %>%
                  group_by(videoname) %>%
                  slice_head(n = 1),
                by = "videoname") %>%
      mutate(Behavioral.category = "All")

    avg_duration_data <- bind_rows(avg_duration_data, avg_duration_data_all)

    tot_duration_data <- behavior_data %>%
      arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
      mutate(
        next_time = lead(Time),
        Duration = next_time - Time
      ) %>%
      filter(Behavior.type == "START") %>%
      group_by(videoname, Behavioral.category) %>%
      dplyr::summarise(tot_duration = sum(Duration), .groups = "drop") %>%
      complete(videoname, Behavioral.category, fill = list(tot_duration = 0))

    tot_duration_data_all <- tot_duration_data %>%
      group_by(videoname) %>%
      dplyr::summarise(tot_duration = sum(tot_duration), .groups = "drop") %>%
      left_join(tot_duration_data %>%
                  select(videoname, Behavioral.category) %>%
                  distinct() %>%
                  group_by(videoname) %>%
                  slice_head(n = 1),
                by = "videoname") %>%
      mutate(Behavioral.category = "All")

    tot_duration_data <- bind_rows(tot_duration_data, tot_duration_data_all)

    # Combine all three tibbles into one
    combined_data <- count_data %>%
      left_join(avg_duration_data, by = c("videoname", "Behavioral.category")) %>%
      left_join(tot_duration_data, by = c("videoname", "Behavioral.category")) %>%
      left_join(count_prop_data, by = c("videoname", "Behavioral.category", "count")) %>%
      complete(videoname, Behavioral.category, fill = list(count_prop = 0)) %>%
      mutate(Behavioral.category = case_when(
        Behavioral.category == "Tolerant/Cooperative" ~ "Cooperative",
        TRUE ~ Behavioral.category
      ))

    # if level is "category", then we are looking at behaviors
  } else if (level == "single") {
    # Create plots for each individual behavior
    count_data <- behavior_data %>%
      group_by(videoname, Behavior) %>%
      filter(Behavior.type == "START") %>% # Only consider START times for counting
      dplyr::summarise(count = n(), .groups = "drop") %>%
      mutate(Behavior = case_when( # get rid of any hyphens or spaces in behavior names
        Behavior == "c-posture" ~ "cposture",
        Behavior == "u-turn" ~ "uturn",
        Behavior == "head-to-body" ~ "headtobody",
        Behavior == "tandem walking" ~ "tandemwalking",
        Behavior == "head-to-head" ~ "headtohead",
        Behavior == "side-by-side" ~ "sidebyside",
        Behavior == "attempted pass" ~ "attemptedpass",
        TRUE ~ Behavior
      )) %>%
      complete(videoname, Behavior, fill = list(count = 0))

    avg_duration_data <- behavior_data %>%
      arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
      mutate(
        next_time = lead(Time),
        Duration = next_time - Time
      ) %>%
      filter(Behavior.type == "START") %>%
      group_by(videoname, Behavior) %>%
      dplyr::summarise(avg_duration = mean(Duration), .groups = "drop") %>%
      mutate(Behavior = case_when( # get rid of any hyphens or spaces in behavior names
        Behavior == "c-posture" ~ "cposture",
        Behavior == "u-turn" ~ "uturn",
        Behavior == "head-to-body" ~ "headtobody",
        Behavior == "tandem walking" ~ "tandemwalking",
        Behavior == "head-to-head" ~ "headtohead",
        Behavior == "side-by-side" ~ "sidebyside",
        Behavior == "attempted pass" ~ "attemptedpass",
        TRUE ~ Behavior
      )) %>%
      complete(videoname, Behavior, fill = list(avg_duration = 0))

    tot_duration_data <- behavior_data %>%
      arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
      mutate(
        next_time = lead(Time),
        Duration = next_time - Time
      ) %>%
      filter(Behavior.type == "START") %>%
      group_by(videoname, Behavior) %>%
      dplyr::summarise(tot_duration = sum(Duration), .groups = "drop") %>%
      mutate(Behavior = case_when( # get rid of any hyphens or spaces in behavior names
        Behavior == "c-posture" ~ "cposture",
        Behavior == "u-turn" ~ "uturn",
        Behavior == "head-to-body" ~ "headtobody",
        Behavior == "tandem walking" ~ "tandemwalking",
        Behavior == "head-to-head" ~ "headtohead",
        Behavior == "side-by-side" ~ "sidebyside",
        Behavior == "attempted pass" ~ "attemptedpass",
        TRUE ~ Behavior
      )) %>%
      complete(videoname, Behavior, fill = list(tot_duration = 0))

    # Combine all three tibbles into one
    combined_data <- count_data %>%
      left_join(avg_duration_data, by = c("videoname", "Behavior")) %>%
      left_join(tot_duration_data, by = c("videoname", "Behavior"))
  }

  return(combined_data)
}

caste_data_pipeline <- function(behavior_data, level) {

  if (level == "category") {

    # 1. Extract unique subjects and map Caste based on Specimen.ID, Tag_Color, and videoname
    unique_subjects <- master_data %>%
      select(Videoname, Specimen.ID, Tag.Color, Caste) %>%
      distinct(Videoname, Specimen.ID, Tag.Color, Caste) %>%
      rename(videoname = Videoname, Tag_Color = Tag.Color)

    # Extract counts and durations for each caste
    count_data <- behavior_data %>%
      filter(Behavior.type == "START") %>%
      mutate(Tag_Color = case_when(
        Subject == "Black Tag" ~ "B",
        Subject == "Purple Tag" ~ "P",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Tag_Color)) %>% # this line gets rid of any un-directed interactions
      left_join(master_data, by = c("videoname" = "Videoname", "Tag_Color" = "Tag.Color")) %>%
      select(Subject, Behavioral.category, Specimen.ID, Caste) %>%
      group_by(Specimen.ID, Behavioral.category, Caste) %>%
      dplyr::summarise(count = n(), .groups = "drop") %>%
      complete(Specimen.ID = unique(master_data$Specimen.ID), 
              Behavioral.category = unique(behavior_data$Behavioral.category),
              fill = list(count = 0)) %>%
      group_by(Specimen.ID) %>%
      fill(Caste, .direction = "downup") %>%
      # Join with unique_subjects to get the correct Caste values
      left_join(unique_subjects, by = c("Specimen.ID")) %>%

      # Now, fill the Caste based on the join, ensuring the correct Caste is mapped
      mutate(Caste = coalesce(Caste.y, Caste.x)) %>%  # Prefer Caste from the join over initial values
      select(-Caste.x, -Caste.y, -Tag_Color, -videoname) # Remove duplicate columns

    # 3. Merge behavior data and fill Caste based on unique_subjects
    count_prop_data <- count_data %>%

      group_by(Specimen.ID) %>%
      mutate(sum_count = sum(count)) %>%  # Calculate total count for Specimen.ID
      mutate(count_prop_caste = ifelse(sum_count == 0, 0, count / sum_count)) %>%  # Calculate proportions
      
      select(-sum_count) %>%
      ungroup()

    # Add a behavioral category for all interactions summing counts by Specimen ID
    count_data_all <- count_data %>%
      group_by(Specimen.ID) %>%
      dplyr::summarise(count = sum(count), .groups = "drop") %>%
      left_join(count_data %>%
                  select(Specimen.ID, Caste) %>%
                  distinct() %>%
                  group_by(Specimen.ID) %>%
                  slice_head(n = 1),
                by = "Specimen.ID") %>%
      mutate(Behavioral.category = "All")

    count_data <- bind_rows(count_data, count_data_all)

    avg_duration_data <- behavior_data %>%
      arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
      mutate(
        next_time = lead(Time),
        Duration = next_time - Time
      ) %>%
      filter(Behavior.type == "START") %>%
      mutate(Tag_Color = case_when(
        Subject == "Black Tag" ~ "B",
        Subject == "Purple Tag" ~ "P",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Tag_Color)) %>%
      left_join(master_data, by = c("videoname" = "Videoname", "Tag_Color" = "Tag.Color")) %>%
      select(Subject, Behavior, Behavioral.category, Behavior.type, Time, Image.index, videoname, Specimen.ID, Caste, Duration) %>%
      group_by(Specimen.ID, Behavioral.category, Caste) %>%
      dplyr::summarise(avg_duration = mean(Duration), .groups = "drop") %>%
      complete(Specimen.ID = unique(master_data$Specimen.ID), 
              Behavioral.category = unique(behavior_data$Behavioral.category),
              fill = list(avg_duration = 0)) %>%
      group_by(Specimen.ID) %>%
      fill(Caste, .direction = "downup") %>%
      # Join with unique_subjects to get the correct Caste values
      left_join(unique_subjects, by = c("Specimen.ID")) %>%

      # Now, fill the Caste based on the join, ensuring the correct Caste is mapped
      mutate(Caste = coalesce(Caste.y, Caste.x)) %>%  # Prefer Caste from the join over initial values
      select(-Caste.x, -Caste.y, -Tag_Color, -videoname) # Remove duplicate columns

    # Add a behavioral category for all interactions summing total duration by Specimen ID
    avg_duration_data_all <- avg_duration_data %>%
      group_by(Specimen.ID) %>%
      dplyr::summarise(avg_duration = sum(avg_duration), .groups = "drop") %>%
      left_join(avg_duration_data %>%
                  select(Specimen.ID, Caste) %>%
                  distinct() %>%
                  group_by(Specimen.ID) %>%
                  slice_head(n = 1),
                by = "Specimen.ID") %>%
      mutate(Behavioral.category = "All")

    avg_duration_data <- bind_rows(avg_duration_data, avg_duration_data_all)

    tot_duration_data <- behavior_data %>%
      arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
      mutate(
        next_time = lead(Time),
        Duration = next_time - Time
      ) %>%
      filter(Behavior.type == "START") %>%
      mutate(Tag_Color = case_when(
        Subject == "Black Tag" ~ "B",
        Subject == "Purple Tag" ~ "P",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Tag_Color)) %>%
      left_join(master_data, by = c("videoname" = "Videoname", "Tag_Color" = "Tag.Color")) %>%
      select(Subject, Behavior, Behavioral.category, Behavior.type, Time, Image.index, videoname, Specimen.ID, Caste, Duration) %>%
      group_by(Specimen.ID, Behavioral.category, Caste) %>%
      dplyr::summarise(tot_duration = sum(Duration), .groups = "drop") %>%
      complete(Specimen.ID = unique(master_data$Specimen.ID), 
              Behavioral.category = unique(behavior_data$Behavioral.category),
              fill = list(tot_duration = 0)) %>%
      group_by(Specimen.ID) %>%
      fill(Caste, .direction = "downup") %>%
      # Join with unique_subjects to get the correct Caste values
      left_join(unique_subjects, by = c("Specimen.ID")) %>%

      # Now, fill the Caste based on the join, ensuring the correct Caste is mapped
      mutate(Caste = coalesce(Caste.y, Caste.x)) %>%  # Prefer Caste from the join over initial values
      select(-Caste.x, -Caste.y, -Tag_Color, -videoname) # Remove duplicate columns

    # Add a behavioral category for all interactions summing total duration by Specimen ID
    tot_duration_data_all <- tot_duration_data %>%
      group_by(Specimen.ID) %>%
      dplyr::summarise(tot_duration = sum(tot_duration), .groups = "drop") %>%
      left_join(tot_duration_data %>%
                  select(Specimen.ID, Caste) %>%
                  distinct() %>%
                  group_by(Specimen.ID) %>%
                  slice_head(n = 1),
                by = "Specimen.ID") %>%
      mutate(Behavioral.category = "All")

    tot_duration_data <- bind_rows(tot_duration_data, tot_duration_data_all)

    # Combine all three tibbles into one
    combined_data <- count_data %>%
      left_join(avg_duration_data, by = c("Specimen.ID", "Behavioral.category", "Caste")) %>%
      left_join(tot_duration_data, by = c("Specimen.ID", "Behavioral.category", "Caste")) %>%
      left_join(count_prop_data, by = c("Specimen.ID", "Behavioral.category", "Caste", "count")) %>%
      
      # Ungroup to avoid grouping issues with complete()
      ungroup() %>%
      
      # Use complete to fill missing combinations of Specimen.ID, Behavioral.category, and Caste
      complete(Specimen.ID, Behavioral.category, Caste, fill = list(count_prop_caste = 0)) %>%
      
      filter(!is.na(count) & !is.na(avg_duration) & !is.na(tot_duration) & !is.na(count_prop_caste)) %>%
      
      mutate(Behavioral.category = case_when(
        Behavioral.category == "Tolerant/Cooperative" ~ "Cooperative",
        TRUE ~ Behavioral.category
      ))


  } else if (level == "single") {

    # Create plots for each individual behavior
    count_data <- behavior_data %>%
      filter(Behavior.type == "START") %>%
      mutate(Tag_Color = case_when(
        Subject == "Black Tag" ~ "B",
        Subject == "Purple Tag" ~ "P",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Tag_Color)) %>% # this line gets rid of any un-directed interactions
      left_join(master_data, by = c("videoname" = "Videoname", "Tag_Color" = "Tag.Color")) %>%
      select(Subject, Behavior, Specimen.ID, Caste) %>%
      group_by(Specimen.ID, Behavior, Caste) %>%
      dplyr::summarise(count = n(), .groups = "drop") %>%
      mutate(Behavior = case_when( # get rid of any hyphens or spaces in behavior names
        Behavior == "c-posture" ~ "cposture",
        Behavior == "u-turn" ~ "uturn",
        Behavior == "head-to-body" ~ "headtobody",
        Behavior == "tandem walking" ~ "tandemwalking",
        Behavior == "head-to-head" ~ "headtohead",
        Behavior == "side-by-side" ~ "sidebyside",
        Behavior == "attempted pass" ~ "attemptedpass",
        TRUE ~ Behavior
      )) %>%
      complete(Specimen.ID, Behavior, fill = list(count = 0)) %>%
      group_by(Specimen.ID) %>%
      fill(Caste, .direction = "downup")

    avg_duration_data <- behavior_data %>%
      arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
      mutate(
        next_time = lead(Time),
        Duration = next_time - Time
      ) %>%
      filter(Behavior.type == "START") %>%
      mutate(Tag_Color = case_when(
        Subject == "Black Tag" ~ "B",
        Subject == "Purple Tag" ~ "P",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Tag_Color)) %>% # this line gets rid of any un-directed interactions
      left_join(master_data, by = c("videoname" = "Videoname", "Tag_Color" = "Tag.Color")) %>%
      select(Subject, Behavior, Specimen.ID, Caste, Duration) %>%
      group_by(Specimen.ID, Behavior, Caste) %>%
      dplyr::summarise(avg_duration = mean(Duration), .groups = "drop") %>%
      mutate(Behavior = case_when( # get rid of any hyphens or spaces in behavior names
        Behavior == "c-posture" ~ "cposture",
        Behavior == "u-turn" ~ "uturn",
        Behavior == "head-to-body" ~ "headtobody",
        Behavior == "tandem walking" ~ "tandemwalking",
        Behavior == "head-to-head" ~ "headtohead",
        Behavior == "side-by-side" ~ "sidebyside",
        Behavior == "attempted pass" ~ "attemptedpass",
        TRUE ~ Behavior
      )) %>%
      complete(Specimen.ID, Behavior, fill = list(avg_duration = 0)) %>%
      group_by(Specimen.ID) %>%
      fill(Caste, .direction = "downup")

    tot_duration_data <- behavior_data %>%
      arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
      mutate(
        next_time = lead(Time),
        Duration = next_time - Time
      ) %>%
      filter(Behavior.type == "START") %>%
      mutate(Tag_Color = case_when(
        Subject == "Black Tag" ~ "B",
        Subject == "Purple Tag" ~ "P",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Tag_Color)) %>% # this line gets rid of any un-directed interactions
      left_join(master_data, by = c("videoname" = "Videoname", "Tag_Color" = "Tag.Color")) %>%
      select(Subject, Behavior, Specimen.ID, Caste, Duration) %>%
      group_by(Specimen.ID, Behavior, Caste) %>%
      dplyr::summarise(tot_duration = sum(Duration), .groups = "drop") %>%
      mutate(Behavior = case_when( # get rid of any hyphens or spaces in behavior names
        Behavior == "c-posture" ~ "cposture",
        Behavior == "u-turn" ~ "uturn",
        Behavior == "head-to-body" ~ "headtobody",
        Behavior == "tandem walking" ~ "tandemwalking",
        Behavior == "head-to-head" ~ "headtohead",
        Behavior == "side-by-side" ~ "sidebyside",
        Behavior == "attempted pass" ~ "attemptedpass",
        TRUE ~ Behavior
      )) %>%
      complete(Specimen.ID, Behavior, fill = list(tot_duration = 0)) %>%
      group_by(Specimen.ID) %>%
      fill(Caste, .direction = "downup")

    # Combine all three tibbles into one
    combined_data <- count_data %>%
      left_join(avg_duration_data, by = c("Specimen.ID", "Behavior", "Caste")) %>%
      left_join(tot_duration_data, by = c("Specimen.ID", "Behavior", "Caste"))

  }

  return(combined_data)

}

prepare_lmm_data <- function(data, measures, metric, contrast_list, video_list, nest_site_ids) {
  behavior_col <- if ("Behavioral.category" %in% colnames(data)) {
    "Behavioral.category"

  } else if ("Behavior" %in% colnames(data)) {
    "Behavior"

  }

  behavior_lists <- list()

  for (i in seq_along(measures)) {
    behavior_list <- data %>%
      filter(!!sym(behavior_col) == measures[i]) %>%
      pull(metric)

    behavior_lists[[i]] <- behavior_list
  }

  lmm_df <- data.frame(
    contrast = factor(contrast_list, levels = unique(contrast_list)),
    videoname = video_list,
    nest_site_id = nest_site_ids
  )

  for (i in seq_along(behavior_lists)) {
    lmm_df[[paste0("beh_", i)]] <- behavior_lists[[i]]
  }

  lmm_df$date <- sapply(lmm_df$videoname, function(x) strsplit(x, "_")[[1]][1])

  return(lmm_df)
}

prepare_lmm_prop_data <- function(data, measures, metric) {
  behavior_lists <- list()

  for (i in seq_along(measures)) {
    behavior_list <- data %>%
      filter(Behavioral.category == measures[i]) %>%
      pull(metric)

    behavior_lists[[i]] <- behavior_list
  }

  # Apply the same logic using distinct() in the final pipeline
  lmm_df <- data %>%
    # Add the date column from videoname
    mutate(date = substr(videoname, 1, 8)) %>%
    
    # Group by videoname and create other Specimen.ID based on min and max Specimen.ID within each videoname
    group_by(videoname) %>%
    mutate(
      other_specimen_id = if_else(Specimen.ID == min(Specimen.ID), 
                                  max(Specimen.ID), min(Specimen.ID)),

      Caste_other = if_else(other_specimen_id == min(Specimen.ID), 
                            first(Caste), last(Caste))
    ) %>%

    mutate(
      contrast = case_when(
        # Check if current caste is queen and the other specimen is solitary
        (Caste == "queen" & Caste_other == "solitary") |

        # Check if current caste is solitary and the other specimen is queen
        (Caste == "solitary" & Caste_other == "queen") ~ "queen_solitary",

        # Check if current caste is queen and the other specimen is worker
        (Caste == "queen" & Caste_other == "worker") |

        # Check if current caste is worker and the other specimen is queen
        (Caste == "worker" & Caste_other == "queen") ~ "queen_worker",

        # Other cases (e.g., queen_queen or worker_worker)
        TRUE ~ paste0(Caste, "_", Caste)
      )
    ) %>%
    ungroup() %>%

    # Remove Behavioral.category column and other unnecessary steps
    select(-Behavioral.category, -other_specimen_id, -Caste_other, -!!sym(metric)) %>%

    # Group and arrange the final dataframe
    arrange(Specimen.ID, date) %>%
 
    distinct(Specimen.ID, Caste, date, contrast) %>%

    mutate(Caste = factor(Caste, levels = c("queen", "worker", "solitary")),
           contrast = factor(contrast, levels = c("queen_queen", "queen_solitary", "queen_worker", "solitary_solitary", "worker_worker")))

  for (i in seq_along(behavior_lists)) {
    lmm_df[[paste0("beh_", i)]] <- behavior_lists[[i]]
  }

  return(lmm_df)
}