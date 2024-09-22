# Load constants and utility functions
source("scripts/R/constants.R")
source("scripts/R/utils.R")

# Install and load required packages
install_if_missing(REQUIRED_PACKAGES)
invisible(lapply(REQUIRED_PACKAGES, library, character.only = TRUE))

# Load data
master_data <- load_master_data(BASE_DIR_MASTER_DATA)
video_data <- create_video_df(master_data, "data/video_df.csv")$video_df

# Extract video and contrast lists
video_list <- video_data$videoname
contrast_list <- video_data$contrast
contrast_list <- gsub(contrast_list, pattern = "-", replacement = "_")
nest_site_ids <- video_data$nest_ID_contrast


# Generate file paths for velocity data
video_list_vels <- generate_vels_file_paths(video_list, contrast_list, BASE_DIR_VELOCITIES)

# Calculate motion proportions
motion_prop_list <- calculate_motion_proportion(video_list_vels)

# Load bout counts
bout_counts <- load_bout_counts(video_list, contrast_list, BASE_DIR_BATCH_DEGREE)
hth_bout_list <- bout_counts$hth_bouts
htb_bout_list <- bout_counts$htb_bouts

# Prepare data for analysis
lmm_df <- prepare_lmm_data(
  motion_prop_list, hth_bout_list, htb_bout_list, contrast_list, video_list, nest_site_ids
)

# Define the measures to analyze
measures <- c("motion_prop", "hth_bouts", "htb_bouts")

# Loop over each measure, perform analysis and plotting
for (measure in measures) {
  # Perform Linear Mixed Model analysis with lmer
  lmm <- lmer(
    as.formula(paste(measure, "~ contrast + (1 | date ) + (1 | nest_site_id)")),
    data = lmm_df
  )

  # Output ANOVA results using car::Anova for lmer
  cat("\nANOVA for", measure, ":\n")
  print(car::Anova(lmm))

  # Estimated Marginal Means (EMMs) and pairwise comparisons
  emmeans_result <- emmeans(lmm, ~contrast)
  pairwise_contrasts <- pairs(emmeans_result, adjust = "sidak")

  # Output pairwise comparisons
  cat("\nPairwise Comparisons for", measure, ":\n")
  print(summary(pairwise_contrasts, infer = TRUE))

  # Generate compact letter display for significance
  cld_result <- cld(emmeans_result, Letters = letters, adjust = "sidak")
  emm_df <- as.data.frame(emmeans_result)
  emm_df$Letters <- cld_result$.group

  # Define labels and titles for plots
  y_label <- switch(measure,
    "motion_prop" = "Estimated Motion Proportion",
    "hth_bouts" = "Estimated HTH Bouts",
    "htb_bouts" = "Estimated HTB Bouts"
  )
  plot_title <- switch(measure,
    "motion_prop" = "Motion Proportion by Contrast",
    "hth_bouts" = "HTH Bouts by Contrast",
    "htb_bouts" = "HTB Bouts by Contrast"
  )

  # Create and display the plot
  emm_plot <- ggplot(emm_df, aes(x = contrast, y = emmean, color = contrast)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
    geom_text(aes(label = Letters), vjust = -0.5, hjust = -0.3, size = 5, color = "black") +
    scale_color_manual(values = CONTRAST_COLORS) +
    labs(
      title = plot_title,
      x = "Social Contrast",
      y = y_label
    ) +
    SHARED_THEME

  print(emm_plot)

  # Save the plot to file
  plot_filename <- paste0("figures/emm_", measure, ".jpeg")
  ggsave(plot_filename, emm_plot, width = 8, height = 6, dpi = 300)
}
