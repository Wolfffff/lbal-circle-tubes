# analysis.R
source("scripts/R/constants.R")
source("scripts/R/utils.R")
source("scripts/R/plots.R")

# Install required packages
install_if_missing(required_packages)

# Load master data
master_data <- load_master_data(base_dir_master_data)

video_df <- create_video_df(master_data)

# Prepare caste data
plot_df <- prepare_caste_data(master_data)

# Plot ovarian development per caste
plot_ovarian_development(plot_df, plot_colors)

# Save the ovarian development plot
save_plot("figures/ovarian_development.png")

video_data <- load_video_list_and_contrast("data/processed_data")
video_list <- video_data$video_list
contrast_list <- video_data$contrast_list

# Generate vels file paths
video_list_vels <- generate_vels_file_paths(video_list, contrast_list, "data/processed_data/velocities")

# Calculate motion proportion
motion_prop_list <- calculate_motion_proportion(video_list_vels)

# Now, you can proceed with the rest of the analysis, such as LMM and plotting
# Load bout data
bout_data <- load_bout_data(
  video_list_hth = paste0(gsub(".txt", "_hth_bouts.csv", video_list)),
  video_list_htb = paste0(gsub(".txt", "_htb_bouts.csv", video_list))
)

# Prepare LMM data
lmm_df <- prepare_lmm_data(motion_prop_list, bout_data$hth_bout_list, bout_data$htb_bout_list, contrast_list, video_df)

# Perform LMM analysis
lmm <- lme(motion_prop ~ contrast, random = list(date = ~1, videoname.nest_ID_contrast = ~1), data = lmm_df)
anova(lmm)
summary(lmm)

# Post hoc Tukey test
post_hoc <- glht(lmm, linfct = mcp(contrast = "Tukey"))
summary(post_hoc)

# Plot motion and interaction results
plot_motion_interactions(lmm_df, plot_colors)

# Save the motion and interaction plot
save_plot("figures/motion_interactions.png")
