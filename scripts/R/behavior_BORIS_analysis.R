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

# Plot proportion data
prop_data <- prop_data_pipeline(behavior_data)

measures <- c("Aggressive", "Avoidant", "Neutral", "Cooperative")

metrics <- c("count_prop_caste")

for (metric in metrics) {
  # Prepare data for linear mixed model (LMM) analysis
  lmm_df <- prepare_lmm_prop_data(
    prop_data, measures, metric
  )

  # Define the formula for the linear mixed model (LMM) analysis
  formula <- " ~ contrast + (1 | date)"

  plot_by_contrast(lmm_df, formula, metric, measures)
}

### Start Contrast Analysis Here ###*****************************************************************************************************

#plot_scatters(combined_data)

# Categories: regular_categories or proportion_categories
# Singles: singles_aggr, singles_avoi, singles_neut, singles_coop
plot_type = "regular_categories" 

params <- choose_plot_type(plot_type)

combined_data <- contrast_data_pipeline(behavior_data, params$level) # level parameter is either "category" or "single"

# Extract the values for each metric and plot
for (metric in params$metrics) {
  # Prepare data for linear mixed model (LMM) analysis
  lmm_df <- prepare_lmm_data(
    combined_data, params$measures, metric, contrast_list, video_list, nest_site_ids
  )

  # Define the formula for the linear mixed model (LMM) analysis
  formula <- " ~ contrast + (1 | date)"

  plot_by_contrast(lmm_df, formula, metric, measures)
}


### Individual Analysis Here ###**********************************************************************************************************

# Categories: regular_categories or proportion_categories
# Singles: singles_aggr, singles_avoi, singles_neut, singles_coop
plot_type <- "proportion_categories" 

params <- choose_plot_type(plot_type)

combined_data <- caste_data_pipeline(behavior_data, params$level)

# Extract the values for each metric and plot
for (metric in metrics) {

  plot_by_caste(combined_data, metric, measures)

}


# Convert data to proportions then perform clr(), but only if behavioral categories are mutually exclusive**
# Look into correlations between morphology and behavior***
# Try plotting and analyzing just the residuals (CLD wouldn't change)*
# Separate nestmates and non-nestmates for Q-W and W-W***
# Try looking at individual behaviors in ethogram***, then look into re-labeling tandem walking into new behavioral categories
# Look into correlations between aggressive and cooperative interactions across videos, may need to standardize in some way**
# Try making stacked bar chart with a bar for each video*