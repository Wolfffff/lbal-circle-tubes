# plot_utils.R

# Load required packages
source("constants.R")
require(stringr)
require(dplyr)
require(tidyr)
require(locits)
require(rstatix)
require(nlme)
require(lme4)
require(multcomp)
require(reshape2)
require(emmeans)
require(ggpubr)
require(ggplot2)
require(patchwork)

# Function to check and install missing packages using pak
install_if_missing <- function(packages) {
  # Ensure pak is installed
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }

  # Find packages that are not installed
  missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]

  # Install missing packages using pak
  if (length(missing_packages)) {
    pak::pkg_install(missing_packages)
  }

  # Load the required packages
  invisible(lapply(packages, library, character.only = TRUE))
}

# Run the function to install and load packages
install_if_missing(REQUIRED_PACKAGES)

# Function to plot behavioral metrics by contrast
plot_by_contrast <- function(lmm_df, formula, plot_dir) {
  # Define measures for analysis (these shuold match the column names in lmm_df, see utils.R)
  measures <- c("aggr_ints", "avoi_ints", "neut_ints", "coop_ints", "all_ints")

  # Create a list to store the plots and initialize a counter
  plotlist <- list()
  i <- 1

  # Loop over each measure to perform analysis and plotting
  for (measure in measures) {
    # Perform Linear Mixed Model (LMM) analysis using lmer
    # Date is a fixed effect and nest side id is a random intercept effect
    lmm <- lmer(
      as.formula(paste(measure, formula)),
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

    # Define labels, titles, and/or y-values based on the measure
    plot_title <- switch(measure,
      "aggr_ints" = "Aggressive Interactions",
      "avoi_ints" = "Avoidant Interactionst",
      "neut_ints" = "Neutral Interactions",
      "coop_ints" = "Cooperative Interactions",
      "all_ints" = "All Interactions"
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
        y = "# of Interacions"
      ) +
      SHARED_THEME

    # Add the plot to the list of plots and increment the counter
    plotlist[[i]] <- box_plots
    i <- i + 1
  }

  # Save the plot to the specified directory
  whole_plot <- (plotlist[[1]] | plotlist[[2]]) / (plotlist[[3]] | plotlist[[4]]) / plotlist[[5]]
  ggsave(plot_dir, whole_plot, width = 8, height = 6, dpi = 300)
  
}

# Function to plot behavioral metrics by caste
plot_by_caste <- function(behavior_data_grouped, plot_dir) {



}
