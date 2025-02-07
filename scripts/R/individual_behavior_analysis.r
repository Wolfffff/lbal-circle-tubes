# Load constants and utility functions
source("constants.R")
source("utils.R")

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

### Total Duration of Each Interaction Type ###
behavior_data_grouped <- behavior_data %>%
  arrange(videoname, Subject, Behavior, Time) %>%
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
  select(Subject, Behavioral.category, Specimen.ID, Caste, Duration) %>%
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

behavior_data_grouped <- bind_rows(behavior_data_grouped, all_interactions) %>%
  arrange(Specimen.ID, Behavioral.category)

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
  stats_df <- plot_df %>%
    pairwise_t_test(formula = tot_duration ~ Caste, p.adjust.method = "bonferroni")

  # Filter out non-significant comparisons and select only significant group pairs
  significant_comparisons <- stats_df %>%
    filter(p.adj <= 0.05) %>%
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