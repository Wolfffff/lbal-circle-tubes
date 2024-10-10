# Load constants and utility functions
source("scripts/R/constants.R")
source("scripts/R/utils.R")

# Install and load required packages
install_if_missing(REQUIRED_PACKAGES)
invisible(lapply(REQUIRED_PACKAGES, library, character.only = TRUE))

# Load data
master_data <- load_master_data(BASE_DIR_MASTER_DATA)
video_data <- create_video_df(master_data, "data/video_df.csv")$video_df

# Extract relevant data columns
video_list <- video_data$videoname
contrast_list <- gsub(video_data$contrast, pattern = "-", replacement = "_")
nest_site_ids <- video_data$nest_ID_contrast

# Generate file paths for velocity data
video_list_vels <- generate_vels_file_paths(video_list, contrast_list, BASE_DIR_VELOCITIES)

# Calculate motion proportions
motion_prop_list <- calculate_motion_proportion(video_list_vels)

# Load bout counts data (HTH and HTB)
bout_counts <- load_bout_counts(video_list, contrast_list, BASE_DIR_BATCH_DEGREE)
hth_bout_list <- bout_counts$hth_bouts
htb_bout_list <- bout_counts$htb_bouts

# Prepare data for linear mixed model (LMM) analysis
lmm_df <- prepare_lmm_data(
  motion_prop_list, hth_bout_list, htb_bout_list, contrast_list, video_list, nest_site_ids
)

# Define measures for analysis
measures <- c("motion_prop", "hth_bouts", "htb_bouts")

# Loop over each measure to perform analysis and plotting
for (measure in measures) {
  # Perform Linear Mixed Model (LMM) analysis using lmer
  # Date is a fixed effect and nest side id is a random intercept effect
  lmm <- lmer(
    as.formula(paste(measure, "~ contrast * date + (1 + contrast | nest_site_id)")),
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
    "motion_prop" = "Estimated Motion Proportion",
    "hth_bouts" = "Estimated HTH Bouts",
    "htb_bouts" = "Estimated HTB Bouts"
  )
  plot_title <- switch(measure,
    "motion_prop" = "Motion Proportion by Contrast",
    "hth_bouts" = "HTH Bouts by Contrast",
    "htb_bouts" = "HTB Bouts by Contrast"
  )

  # Create plot for the current measure
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

  # Display the plot
  print(emm_plot)

  # Save the plot as a file
  plot_filename <- paste0("figures/emm_", measure, ".jpeg")
  ggsave(plot_filename, emm_plot, width = 8, height = 6, dpi = 300)
}
