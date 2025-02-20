# Load constants and utility functions
source("constants.R")
source("utils.R")
source("plot_utils.R")

# Install and load required packages
install_if_missing(REQUIRED_PACKAGES)
invisible(lapply(REQUIRED_PACKAGES, library, character.only = TRUE))

# Load data
master_data <- load_master_data(BASE_DIR_MASTER_DATA)
behavior_data <- load_behavior_data(BASE_DIR_MASTER_DATA)
video_data <- create_video_df(master_data, "../../data/video_df.csv")$video_df

# Sort video_data by videoname to match with grouped behavior_data later
video_data <- video_data[order(video_data$videoname), ]

# Extract relevant data columns
video_list <- video_data$videoname
contrast_list <- gsub(video_data$contrast, pattern = "-", replacement = "_")
nest_site_ids <- video_data$nest_ID_contrast

### Pre-Analsysis Here ###***************************************************************************************************************
# Create histograms
plot_hists(video_data)

### Start Contrast Analysis Here ###*****************************************************************************************************
# Extract counts and durations for each social contrast
count_data <- behavior_data %>%
  group_by(videoname, Behavioral.category) %>%
  filter(Behavior.type == "START") %>% # Only consider START times for counting
  dplyr::summarise(count = n(), .groups = "drop") %>%
  complete(videoname, Behavioral.category, fill = list(count = 0))

log_count_data <- count_data %>%
  mutate(log_count = log(count + 1e-6))

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

# Combine all three tibbles into one
combined_data <- log_count_data %>%
  left_join(avg_duration_data, by = c("videoname", "Behavioral.category")) %>%
  left_join(tot_duration_data, by = c("videoname", "Behavioral.category"))

metrics <- c("count", "log_count", "avg_duration", "tot_duration")

# Extract the values for each metric and plot
for (metric in metrics) {

  aggressive_list <- combined_data %>%
    filter(Behavioral.category == "Aggressive") %>%
    pull(metric)
  avoidant_list <- combined_data %>%
    filter(Behavioral.category == "Avoidant") %>%
    pull(metric)
  neutral_list <- combined_data %>%
    filter(Behavioral.category == "Neutral") %>%
    pull(metric)
  cooperative_list <- combined_data %>%
    filter(Behavioral.category == "Tolerant/Cooperative") %>%
    pull(metric)

  all_ints_list <- aggressive_list + avoidant_list + neutral_list + cooperative_list

  # Prepare data for linear mixed model (LMM) analysis
  lmm_df <- prepare_lmm_data(
    aggressive_list, avoidant_list, neutral_list, cooperative_list, all_ints_list, contrast_list, video_list, nest_site_ids
  )

  # Define the formula for the linear mixed model (LMM) analysis
  formula <- "~ contrast + (1 | date)"

  plot_by_contrast(lmm_df, formula, metric)
}

### Individual Analysis Here ###**********************************************************************************************************
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
  summarise(count = n(), .groups = "drop") %>%
  complete(Specimen.ID, Behavioral.category, fill = list(count = 0)) %>%
  group_by(Specimen.ID) %>%
  fill(Caste, .direction = "downup")

# Add a behavioral category for all interactions summing counts by Specimen ID
count_data_all <- count_data %>%
  group_by(Specimen.ID) %>%
  summarise(count = sum(count), .groups = "drop") %>%
  left_join(behavior_data_grouped %>%
              select(Specimen.ID, Caste) %>%
              distinct() %>%
              group_by(Specimen.ID) %>%
              slice_head(n = 1),
            by = "Specimen.ID") %>%
  mutate(Behavioral.category = "All Interactions")

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
  summarise(avg_duration = mean(Duration), .groups = "drop") %>%
  complete(Specimen.ID, Behavioral.category, fill = list(avg_duration = 0)) %>%
  group_by(Specimen.ID) %>%
  fill(Caste, .direction = "downup")

# Add a behavioral category for all interactions summing total duration by Specimen ID
avg_duration_data_all <- avg_duration_data %>%
  group_by(Specimen.ID) %>%
  summarise(avg_duration = sum(avg_duration), .groups = "drop") %>%
  left_join(behavior_data_grouped %>%
              select(Specimen.ID, Caste) %>%
              distinct() %>%
              group_by(Specimen.ID) %>%
              slice_head(n = 1),
            by = "Specimen.ID") %>%
  mutate(Behavioral.category = "All Interactions")

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
  summarise(tot_duration = sum(Duration), .groups = "drop") %>%
  complete(Specimen.ID, Behavioral.category, fill = list(tot_duration = 0)) %>%
  group_by(Specimen.ID) %>%
  fill(Caste, .direction = "downup")

# Add a behavioral category for all interactions summing total duration by Specimen ID
tot_duration_data_all <- tot_duration_data %>%
  group_by(Specimen.ID) %>%
  summarise(tot_duration = sum(tot_duration), .groups = "drop") %>%
  left_join(behavior_data_grouped %>%
              select(Specimen.ID, Caste) %>%
              distinct() %>%
              group_by(Specimen.ID) %>%
              slice_head(n = 1),
            by = "Specimen.ID") %>%
  mutate(Behavioral.category = "All Interactions")

tot_duration_data <- bind_rows(tot_duration_data, tot_duration_data_all)

# Combine all three tibbles into one
combined_data <- count_data %>%
  left_join(avg_duration_data, by = c("Specimen.ID", "Behavioral.category", "Caste")) %>%
  left_join(tot_duration_data, by = c("Specimen.ID", "Behavioral.category", "Caste"))

metrics <- c("count", "avg_duration", "tot_duration")

# Extract the values for each metric and plot
for (metric in metrics) {
  # Create the dataframe for plotting
  data_df <- combined_data %>%
    select(Specimen.ID, Behavioral.category, Caste, metric)

  plot_by_caste(data_df, metric)
}

# Convert data to proportions then perform clr(), but only if behavioral categories are mutually exclusive**
# Look into correlations between morphology and behavior***
# Try plotting and analyzing just the residuals (CLD wouldn't change)*
# Separate nestmates and non-nestmates for Q-W and W-W***
# Try looking at individual behaviors in ethogram***, then look into re-labeling tandem walking into new behavioral categories
# Look into correlations between aggressive and cooperative interactions across videos, may need to standardize in some way**
# Try making stacked bar chart with a bar for each video*