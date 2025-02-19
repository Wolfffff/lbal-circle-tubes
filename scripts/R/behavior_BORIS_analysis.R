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
# First extract everything before the first underscore in the videoname column
video_data$date <- sub("_.+", "", video_data$videoname)

# Next create a histogram of the number of videos per day
video_data %>% 
  ggplot(aes(x = date)) +
  geom_bar(stat = "count") +
  labs(
    title = "Number of Videos per Day",
    x = "Date",
    y = "Count"
  ) +
  SHARED_THEME

ggsave("../../figures/date_hist.jpeg", width = 8, height = 6, dpi = 300)

# Next create a histogram of the number of contrasts per day
video_data %>%
  ggplot(aes(x = date, fill = contrast)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(
    title = "Number of Contrasts per Day",
    x = "Date",
    y = "Count"
  ) +
  SHARED_THEME +
  theme(legend.position = "top") # need to override the shared theme here

ggsave("../../figures/date_contrast_hist.jpeg", width = 8, height = 6, dpi = 300)

# Next create a histogram of the number of videos per contrast
video_data %>%
  ggplot(aes(x = contrast)) +
  geom_bar(stat = "count") +
  labs(
    title = "Number of Videos per Contrast",
    x = "Contrast",
    y = "Count"
  ) +
  SHARED_THEME

ggsave("../../figures/contrast_hist.jpeg", width = 8, height = 6, dpi = 300)

### Start Contrast Analysis Here ###*****************************************************************************************************
# Extract counts and durations for each behavioral category
count_data <- behavior_data %>%
  group_by(videoname, Behavioral.category) %>%
  filter(Behavior.type == "START") %>% # Only consider START times for counting
  summarise(count = n(), .groups = "drop") %>%
  complete(videoname, Behavioral.category, fill = list(count = 0))

avg_duration_data <- behavior_data %>%
  arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
  mutate(
    next_time = lead(Time),
    Duration = next_time - Time
  ) %>%
  filter(Behavior.type == "START") %>%
  group_by(videoname, Behavioral.category) %>%
  summarise(avg_duration = mean(Duration), .groups = "drop") %>%
  complete(videoname, Behavioral.category, fill = list(avg_duration = 0))

tot_duration_data <- behavior_data %>%
  arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
  mutate(
    next_time = lead(Time),
    Duration = next_time - Time
  ) %>%
  filter(Behavior.type == "START") %>%
  group_by(videoname, Behavioral.category) %>%
  summarise(tot_duration = sum(Duration), .groups = "drop") %>%
  complete(videoname, Behavioral.category, fill = list(tot_duration = 0))

# Combine all three tibbles into one
combined_data <- count_data %>%
  left_join(avg_duration_data, by = c("videoname", "Behavioral.category")) %>%
  left_join(tot_duration_data, by = c("videoname", "Behavioral.category"))

metrics <- c("count", "avg_duration", "tot_duration")

# Extract the counts for each metric and plot
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

  # Define labels, titles, and/or y-values based on the measure
  plot_title <- switch(metric,
    "count" = "emm_interaction_counts.jpeg",
    "avg_duration" = "emm_average_interaction_durations.jpeg",
    "tot_duration" = "emm_total_interaction_durations.jpeg"
  )

  # Determine the directory and title for the final plot
  plot_dir <- paste0(BASE_DIR_FIGURES, plot_title)

  plot_by_contrast(lmm_df, formula, plot_dir)

}

### Centered Log Ratios instead of proportions? ###

### Individual Analysis Here ###**********************************************************************************************************
# Feed in "behavior_data_grouped" dfs to plot_utils.R by subsetting a combined tibble (should almost mirror previous section)

### Total Number of Interactions for Each Interaction Type ###
# Extract interactions counts for individuals
behavior_data_grouped <- behavior_data %>%
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

behavioral_categories <- unique(behavior_data_grouped$Behavioral.category)

# Create a list to store the plots and initialize a counter
plotlist <- list()
i <- 1

for (category in behavioral_categories) {
  # Prepare the data: filter missing values, select relevant columns, and factorize the "Caste" variable
  plot_df <- behavior_data_grouped %>%
    filter(Behavioral.category == category) %>%
    mutate(Caste = factor(Caste, levels = c("queen", "worker", "solitary")))

  # Conduct pairwise t-tests between groups and adjust p-values using the Bonferroni method
  stats_df <- pairwise.t.test(plot_df$count, plot_df$Caste, p.adjust.method = "bonferroni")

  # Filter out non-significant comparisons and select only significant group pairs
  significant_comparisons <- tidy(stats_df) %>%
    filter(p.value <= 0.05) %>%
    select(group1, group2)

  # Convert the filtered comparisons into a list format suitable for stat_compare_means
  comparisons_list <- if (nrow(significant_comparisons > 0)) {
    lapply(
      split(significant_comparisons, seq.int(nrow(significant_comparisons))),
      function(x) as.character(unlist(x))
    )
  } else {
    list()
  }

  # Create a plot for number of interactions per caste with significance annotations
  box_plots <- ggplot(plot_df, aes(x = Caste, y = count, fill = Caste)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
    geom_jitter(aes(color = Caste), width = 0.15, size = 2, alpha = 0.8) +
    scale_fill_manual(values = CASTE_COLORS) +
    scale_color_manual(values = CASTE_COLORS) +
    labs(
      title = paste0(category, " Interactions per Caste"),
      x = "Caste",
      y = paste0("# of ", category, "Interactions")
    ) +
    stat_compare_means(
      method = "t.test",
      label = "p.signif",
      comparisons = comparisons_list,
      hide.ns = TRUE,
      step.increase = 0.1
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) + # Add more space above the plot
    SHARED_THEME

  # Add the plot to the list of plots and increment the counter
  plotlist[[i]] <- box_plots
  i <- i + 1
}

# Save the plot to the specified directory
whole_plot <- (plotlist[[1]] | plotlist[[2]]) / (plotlist[[3]] | plotlist[[4]])
ggsave("../../figures/individual_behaviors_signif.jpeg", whole_plot, width = 8, height = 6, dpi = 300)

### Total Duration of Each Interaction Type ###
behavior_data_grouped <- behavior_data %>%
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

# Add a behavioral cateogory for all interactions summing total duration by Specimen ID
all_interactions <- behavior_data_grouped %>%
  group_by(Specimen.ID) %>%
  summarise(tot_duration = sum(tot_duration), .groups = "drop") %>%
  left_join(behavior_data_grouped %>%
              select(Specimen.ID, Caste) %>%
              distinct() %>%
              group_by(Specimen.ID) %>%
              slice_head(n = 1),
            by = "Specimen.ID") %>%
  mutate(Behavioral.category = "All Interactions")

behavior_data_grouped <- bind_rows(behavior_data_grouped, all_interactions)

behavioral_categories <- unique(behavior_data_grouped$Behavioral.category)

# Create a list to store the plots and initialize a counter
plotlist <- list()
i <- 1

for (category in behavioral_categories) {
  # Prepare the data: filter missing values, select relevant columns, and factorize the "Caste" variable
  plot_df <- behavior_data_grouped %>%
    filter(Behavioral.category == category) %>%
    mutate(Caste = factor(Caste, levels = c("queen", "worker", "solitary")))

  # Conduct pairwise t-tests between groups and adjust p-values using the Bonferroni method
  stats_df <- pairwise.t.test(plot_df$tot_duration, plot_df$Caste, p.adjust.method = "bonferroni")

  # Filter out non-significant comparisons and select only significant group pairs
  significant_comparisons <- tidy(stats_df) %>%
    filter(p.value <= 0.05) %>%
    select(group1, group2)

  # Convert the filtered comparisons into a list format suitable for stat_compare_means
  comparisons_list <- if (nrow(significant_comparisons > 0)) {
    lapply(
      split(significant_comparisons, seq.int(nrow(significant_comparisons))),
      function(x) as.character(unlist(x))
    )
  } else {
    list()
  }

  # Create a plot for number of interactions per caste with significance annotations
  box_plots <- ggplot(plot_df, aes(x = Caste, y = tot_duration, fill = Caste)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
    geom_jitter(aes(color = Caste), width = 0.15, size = 2, alpha = 0.8) +
    scale_fill_manual(values = CASTE_COLORS) +
    scale_color_manual(values = CASTE_COLORS) +
    labs(
      title = paste0("Duration of ", category, " Interactions per Caste"),
      x = "Caste",
      y = "Total Duration (s)"
    ) +
    stat_compare_means(
      method = "t.test",
      label = "p.signif",
      comparisons = comparisons_list,
      hide.ns = TRUE,
      step.increase = 0.1
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) + # Add more space above the plot
    SHARED_THEME

  # Add the plot to the list of plots and increment the counter
  plotlist[[i]] <- box_plots
  i <- i + 1
}

# Save the plot to the specified directory
whole_plot <- (plotlist[[1]] | plotlist[[2]]) / (plotlist[[3]] | plotlist[[4]]) / plotlist[[5]]
ggsave("../../figures/individual_behavior_durations_signif.jpeg", whole_plot, width = 8, height = 6, dpi = 300)
