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

### Start Contrast Analysis Here ###*****************************************************************************************************
### Total Number of Interactions for Each Interaction Type ###
# Extract counts for each behavioral category
behavior_data_grouped <- behavior_data %>%
  group_by(videoname, Behavioral.category) %>%
  filter(Behavior.type == "START") %>% # Only consider START times for counting
  summarise(count = n(), .groups = "drop") %>%
  complete(videoname, Behavioral.category, fill = list(count = 0))

aggressive_list <- behavior_data_grouped %>%
  filter(Behavioral.category == "Aggressive") %>%
  pull(count)
avoidant_list <- behavior_data_grouped %>%
  filter(Behavioral.category == "Avoidant") %>%
  pull(count)
neutral_list <- behavior_data_grouped %>%
  filter(Behavioral.category == "Neutral") %>%
  pull(count)
cooperative_list <- behavior_data_grouped %>%
  filter(Behavioral.category == "Tolerant/Cooperative") %>%
  pull(count)

# Prepare data for linear mixed model (LMM) analysis
lmm_df <- prepare_lmm_data(
  aggressive_list, avoidant_list, neutral_list, cooperative_list, contrast_list, video_list, nest_site_ids
)

# Define measures for analysis
measures <- c("aggr_ints", "avoi_ints", "neut_ints", "coop_ints")

# Create a list to store the plots and initialize a counter
plotlist <- list()
i <- 1

# Loop over each measure to perform analysis and plotting
for (measure in measures) {
  # Perform Linear Mixed Model (LMM) analysis using lmer
  # Date is a fixed effect and nest side id is a random intercept effect
  lmm <- lmer(
    as.formula(paste(measure, "~ contrast + (1 | date)")),
    data = lmm_df
  )

  # Output ANOVA results using car::Anova for lmer objects
  cat("\nANOVA for", measure, ":\n")
  print(car::Anova(lmm))

  # Calculate estimated marginal means (EMMs) and pairwise contrasts
  emmeans_result <- emmeans(lmm, ~contrast)
  pairwise_contrasts <- pairs(emmeans_result, adjust = "sidak")

  # Output pairwise comparisons
  cat("\nPairwise Comparisons for", measure, ":\n")
  print(summary(pairwise_contrasts, infer = TRUE))

  # Generate compact letter display (CLD) for significance
  cld_result <- cld(emmeans_result, Letters = letters, adjust = "sidak")
  emm_df <- as.data.frame(emmeans_result)
  emm_df$Letters <- cld_result$.group

  # Define labels and titles based on the measure
  y_label <- switch(measure,
    "aggr_ints" = "# of Aggressive Interactions",
    "avoi_ints" = "# of Avoidant Interactions",
    "neut_ints" = "# of Neutral Interactions",
    "coop_ints" = "# of Cooperative Interactions"
  )
  plot_title <- switch(measure,
    "aggr_ints" = "Aggressive Interactions by Contrast",
    "avoi_ints" = "Avoidant Interactions by Contrast",
    "neut_ints" = "Neutral Interactions by Contrast",
    "coop_ints" = "Cooperative Interactions by Contrast"
  )

  y_values <- switch(measure,
    "aggr_ints" = lmm_df$aggr_ints,
    "avoi_ints" = lmm_df$avoi_ints,
    "neut_ints" = lmm_df$neut_ints,
    "coop_ints" = lmm_df$coop_ints,
  )

  # Create plot for the current measure
  box_plots <- ggplot(lmm_df, aes_string(x = "contrast", y = measure, color = "contrast")) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
    geom_jitter(aes(color = contrast), width = 0.15, size = 2, alpha = 0.8) +
    geom_text(data = emm_df, aes(x = contrast, y = upper.CL, label = Letters), 
            vjust = -0.5, hjust = -0.3, size = 5, color = "black") +
    scale_color_manual(values = CONTRAST_COLORS) +
    scale_x_discrete(labels = c(
      "queen_solitary" = "Q-S", "queen_queen" = "Q-Q",
      "solitary_solitary" = "S-S", "queen_worker" = "Q-W",
      "worker_worker" = "W-W"
    )) +
    labs(
      title = plot_title,
      x = "Social Contrast",
      y = y_label
    ) +
    SHARED_THEME

  # Add the plot to the list of plots and increment the counter
  plotlist[[i]] <- box_plots
  i <- i + 1
}

# Save the plot to the specified directory
whole_plot <- (plotlist[[1]] | plotlist[[2]]) / (plotlist[[3]] | plotlist[[4]])
ggsave("../../figures/emm_interaction_counts.jpeg", whole_plot, width = 8, height = 6, dpi = 300)

### Average Duration of Each Interaction Type ###
### Centered Log Ratios instead of proportions? ###

# Extract interaction duration using START and STOP times
behavior_data_grouped <- behavior_data %>%
  arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
  mutate(
    next_time = lead(Time),
    Duration = next_time - Time
  ) %>%
  filter(Behavior.type == "START") %>%
  group_by(videoname, Behavioral.category) %>%
  summarise(avg_duration = mean(Duration), .groups = "drop") %>%
  complete(videoname, Behavioral.category, fill = list(avg_duration = 0))

aggressive_list <- behavior_data_grouped %>% filter(Behavioral.category == "Aggressive") %>% pull(avg_duration)
avoidant_list <- behavior_data_grouped %>% filter(Behavioral.category == "Avoidant") %>% pull(avg_duration)
neutral_list <- behavior_data_grouped %>% filter(Behavioral.category == "Neutral") %>% pull(avg_duration)
cooperative_list <- behavior_data_grouped %>% filter(Behavioral.category == "Tolerant/Cooperative") %>% pull(avg_duration)

# Prepare data for linear mixed model (LMM) analysis
lmm_df <- prepare_lmm_data(
  aggressive_list, avoidant_list, neutral_list, cooperative_list, contrast_list, video_list, nest_site_ids
)

# Define measures for analysis
measures <- c("aggr_ints", "avoi_ints", "neut_ints", "coop_ints")

# Create a list to store the plots and initialize a counter
plotlist <- list()
i <- 1

# Loop over each measure to perform analysis and plotting
for (measure in measures) {
  # Perform Linear Mixed Model (LMM) analysis using lmer
  # Date is a fixed effect and nest side id is a random intercept effect
  lmm <- lmer(
    as.formula(paste(measure, "~ contrast + date + (1 | nest_site_id)")),
    data = lmm_df
  )

  # Output ANOVA results using car::Anova for lmer objects
  cat("\nANOVA for", measure, ":\n")
  print(car::Anova(lmm))

  # Calculate estimated marginal means (EMMs) and pairwise contrasts
  emmeans_result <- emmeans(lmm, ~contrast)
  pairwise_contrasts <- pairs(emmeans_result, adjust = "sidak")

  # Output pairwise comparisons
  cat("\nPairwise Comparisons for", measure, ":\n")
  print(summary(pairwise_contrasts, infer = TRUE))

  # Generate compact letter display (CLD) for significance
  cld_result <- cld(emmeans_result, Letters = letters, adjust = "sidak")
  emm_df <- as.data.frame(emmeans_result)
  emm_df$Letters <- cld_result$.group

  # Define labels and titles based on the measure
  y_label <- switch(measure,
    "aggr_ints" = "Avg Dur. of Aggressive Ints (s)",
    "avoi_ints" = "Avg Dur. of Avoidant Ints (s)",
    "neut_ints" = "Avg Dur. of Neutral Ints (s)",
    "coop_ints" = "Avg Dur. of Cooperative Ints (s)"
  )
  plot_title <- switch(measure,
    "aggr_ints" = "Aggressive Interactions by Contrast",
    "avoi_ints" = "Avoidant Interactions by Contrast",
    "neut_ints" = "Neutral Interactions by Contrast",
    "coop_ints" = "Cooperative Interactions by Contrast"
  )

  y_values <- switch(measure,
    "aggr_ints" = lmm_df$aggr_ints,
    "avoi_ints" = lmm_df$avoi_ints,
    "neut_ints" = lmm_df$neut_ints,
    "coop_ints" = lmm_df$coop_ints,
  )

  # Create plot for the current measure
  box_plots <- ggplot(lmm_df, aes_string(x = "contrast", y = measure, color = "contrast")) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
    geom_jitter(aes(color = contrast), width = 0.15, size = 2, alpha = 0.8) +
    geom_text(data = emm_df, aes(x = contrast, y = upper.CL, label = Letters), 
            vjust = -0.5, hjust = -0.3, size = 5, color = "black") +
    scale_color_manual(values = CONTRAST_COLORS) +
    scale_x_discrete(labels = c(
      "queen_solitary" = "Q-S", "queen_queen" = "Q-Q",
      "solitary_solitary" = "S-S", "queen_worker" = "Q-W",
      "worker_worker" = "W-W"
    )) +
    labs(
      title = plot_title,
      x = "Social Contrast",
      y = y_label
    ) +
    SHARED_THEME

  # Add the plot to the list of plots and increment the counter
  plotlist[[i]] <- box_plots
  i <- i + 1
}

# Save the plot to the specified directory
whole_plot <- (plotlist[[1]] | plotlist[[2]]) / (plotlist[[3]] | plotlist[[4]])
ggsave("../../figures/emm_interaction_durations.jpeg", whole_plot, width = 8, height = 6, dpi = 300)

### Total Duration of Each Interaction Type ###
# Extract interaction duration using START and STOP times
behavior_data_grouped <- behavior_data %>%
  arrange(videoname, Subject, Behavioral.category, Behavior, Time) %>%
  mutate(
    next_time = lead(Time),
    Duration = next_time - Time
  ) %>%
  filter(Behavior.type == "START") %>%
  group_by(videoname, Behavioral.category) %>%
  summarise(tot_duration = sum(Duration), .groups = "drop") %>%
  complete(videoname, Behavioral.category, fill = list(tot_duration = 0))

aggressive_list <- behavior_data_grouped %>% filter(Behavioral.category == "Aggressive") %>% pull(tot_duration)
avoidant_list <- behavior_data_grouped %>% filter(Behavioral.category == "Avoidant") %>% pull(tot_duration)
neutral_list <- behavior_data_grouped %>% filter(Behavioral.category == "Neutral") %>% pull(tot_duration)
cooperative_list <- behavior_data_grouped %>% filter(Behavioral.category == "Tolerant/Cooperative") %>% pull(tot_duration)
all_ints_list <- aggressive_list + avoidant_list + neutral_list + cooperative_list

# Prepare data for linear mixed model (LMM) analysis
lmm_df <- prepare_lmm_data(
  aggressive_list, avoidant_list, neutral_list, cooperative_list, all_ints_list, contrast_list, video_list, nest_site_ids
)

# Define measures for analysis
measures <- c("aggr_ints", "avoi_ints", "neut_ints", "coop_ints", "all_ints")

# Create a list to store the plots and initialize a counter
plotlist <- list()
i <- 1

# Loop over each measure to perform analysis and plotting
for (measure in measures) {
  # Perform Linear Mixed Model (LMM) analysis using lmer
  # Date is a fixed effect and nest side id is a random intercept effect
  lmm <- lmer(
    as.formula(paste(measure, "~ contrast + (1 | date)")),
    data = lmm_df
  )

  # Output ANOVA results using car::Anova for lmer objects
  cat("\nANOVA for", measure, ":\n")
  print(car::Anova(lmm))

  # Calculate estimated marginal means (EMMs) and pairwise contrasts
  emmeans_result <- emmeans(lmm, ~contrast)
  pairwise_contrasts <- pairs(emmeans_result, adjust = "sidak")

  # Output pairwise comparisons
  cat("\nPairwise Comparisons for", measure, ":\n")
  print(summary(pairwise_contrasts, infer = TRUE))

  # Generate compact letter display (CLD) for significance
  cld_result <- cld(emmeans_result, Letters = letters, adjust = "sidak")
  emm_df <- as.data.frame(emmeans_result)
  emm_df$Letters <- cld_result$.group

  # Define labels and titles based on the measure
  y_label <- switch(measure,
    "aggr_ints" = "Total Duration (s)",
    "avoi_ints" = "Total Duration (s)",
    "neut_ints" = "Total Duration (s)",
    "coop_ints" = "Total Duration (s)",
    "all_ints" = "Total Duration (s)"
  )
  plot_title <- switch(measure,
    "aggr_ints" = "Aggressive Interactions by Contrast",
    "avoi_ints" = "Avoidant Interactions by Contrast",
    "neut_ints" = "Neutral Interactions by Contrast",
    "coop_ints" = "Cooperative Interactions by Contrast",
    "all_ints" = "All Interactions by Contrast"
  )

  y_values <- switch(measure,
    "aggr_ints" = lmm_df$aggr_ints,
    "avoi_ints" = lmm_df$avoi_ints,
    "neut_ints" = lmm_df$neut_ints,
    "coop_ints" = lmm_df$coop_ints,
  )

  # Create plot for the current measure
  box_plots <- ggplot(lmm_df, aes_string(x = "contrast", y = measure, color = "contrast")) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
    geom_jitter(aes(color = contrast), width = 0.15, size = 2, alpha = 0.8) +
    geom_text(data = emm_df, aes(x = contrast, y = upper.CL, label = Letters), 
            vjust = -0.5, hjust = -0.3, size = 5, color = "black") +
    scale_color_manual(values = CONTRAST_COLORS) +
    scale_x_discrete(labels = c(
      "queen_solitary" = "Q-S", "queen_queen" = "Q-Q",
      "solitary_solitary" = "S-S", "queen_worker" = "Q-W",
      "worker_worker" = "W-W"
    )) +
    labs(
      title = plot_title,
      x = "Social Contrast",
      y = y_label
    ) +
    SHARED_THEME

  # Add the plot to the list of plots and increment the counter
  plotlist[[i]] <- box_plots
  i <- i + 1
}

# Save the plot to the specified directory
whole_plot <- (plotlist[[1]] | plotlist[[2]]) / (plotlist[[3]] | plotlist[[4]]) / plotlist[[5]]
ggsave("../../figures/emm_total_interaction_durations.jpeg", whole_plot, width = 8, height = 6, dpi = 300)

### Individual Analysis Here ###**********************************************************************************************************
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
  stats_df <- plot_df %>%
    pairwise_t_test(formula = count ~ Caste, p.adjust.method = "bonferroni")

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
