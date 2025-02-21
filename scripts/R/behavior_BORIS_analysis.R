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

# Make a scatter plot of aggressive versus cooperative counts by videoname
count_data %>%
  filter(Behavioral.category %in% c("Aggressive", "Tolerant/Cooperative")) %>%
  pivot_wider(names_from = Behavioral.category, values_from = count) %>%
  ggplot(aes(x = Aggressive, y = `Tolerant/Cooperative`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  stat_cor(method = "pearson", color = "red", label.x.npc = 0.5, label.y.npc = 0.9) +
  labs(
    title = "Aggressive vs. Cooperative Counts by Video",
    x = "Aggressive Interactions",
    y = "Cooperative Interactions"
  ) +
  SHARED_THEME

ggsave("../../figures/agg_coop_scatter.jpeg", width = 8, height = 6, dpi = 300)

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
  mutate(Behavioral.category = case_when(
    Behavioral.category == "Tolerant/Cooperative" ~ "Cooperative",
    TRUE ~ Behavioral.category
  ))

measures <- c("Aggressive", "Avoidant", "Neutral", "Cooperative", "All")

metrics <- c("count", "avg_duration", "tot_duration")

# Extract the values for each metric and plot
for (metric in metrics) {
  behavior_lists <- list()

  for (i in seq_along(measures)) {
    behavior_list <- combined_data %>%
      filter(Behavioral.category == measures[i]) %>%
      pull(metric)

    behavior_lists[[i]] <- behavior_list
  }

  # Prepare data for linear mixed model (LMM) analysis
  lmm_df <- prepare_lmm_data(
    behavior_lists, contrast_list, video_list, nest_site_ids
  )

  # Define the formula for the linear mixed model (LMM) analysis
  formula <- " ~ contrast + (1 | date)"

  plot_by_contrast(lmm_df, formula, metric, measures)
}

# Create plots for each individual behavior
count_data_by_behavior <- behavior_data %>%
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

# Define measures for analysis
measures <- c("cposture", "lunge", "nudge", "bite")

metrics <- c("count")

# Extract the values for each metric and plot
for (metric in metrics) {
  behavior_lists <- list()

  for (i in seq_along(measures)) {
    behavior_list <- count_data_by_behavior %>%
      filter(Behavior == measures[i]) %>%
      pull(metric)

    behavior_lists[[i]] <- behavior_list
  }

  # Prepare data for linear mixed model (LMM) analysis
  lmm_df <- prepare_lmm_data(
    behavior_lists, contrast_list, video_list, nest_site_ids
  )

  # Define the formula for the linear mixed model (LMM) analysis
  formula <- " ~ contrast + (1 | date)"

  plot_by_contrast(lmm_df, formula, metric, measures)
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