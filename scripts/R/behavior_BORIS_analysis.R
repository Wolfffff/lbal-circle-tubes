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

behavior_data_grouped <- behavior_data %>%
  group_by(videoname, Behavioral.category) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(videoname, Behavioral.category, fill = list(count = 0))

aggressive_list <- behavior_data_grouped %>% filter(Behavioral.category == "Aggressive") %>% pull(count)
avoidant_list <- behavior_data_grouped %>% filter(Behavioral.category == "Avoidant") %>% pull(count)
neutral_list <- behavior_data_grouped %>% filter(Behavioral.category == "Neutral") %>% pull(count)
cooperative_list <- behavior_data_grouped %>% filter(Behavioral.category == "Tolerant/Cooperative") %>% pull(count)

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
  box_plots <- ggplot(lmm_df, aes(x = contrast, y = y_values, color = contrast)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
    geom_jitter(aes(color = contrast), width = 0.15, size = 2, alpha = 0.8) +
    geom_text(data = emm_df, aes(x = contrast, y = c(60, 60, 60, 60, 60), label = Letters), vjust = -0.5, hjust = -0.3, size = 5, color = "black") +
    scale_color_manual(values = CONTRAST_COLORS) +
    scale_x_discrete(labels = c("queen_solitary" = "Q-S", "queen_queen" = "Q-Q",
                                "solitary_solitary" = "S-S", "queen_worker" = "Q-W",
                                "worker_worker" = "W-W")) +
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
