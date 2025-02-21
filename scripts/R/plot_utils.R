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

plot_hists <- function(video_data) {
  # First, extract everything before the first underscore in the videoname column
  video_data$date <- sub("_.+", "", video_data$videoname)

  # Next, create a histogram of the number of videos per day
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

  # Next, create a histogram of the number of contrasts per day
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

  # Next, create a histogram of the number of videos per contrast
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
}

# Function to plot behavioral metrics by contrast
plot_by_contrast <- function(lmm_df, formula, metric, measures) {
  # Create a list to store the plots and initialize a counter
  plotlist <- list()
  i <- 1

  # Loop over each measure to perform analysis and plotting
  for (i in seq_along(measures)) {
    # Perform Linear Mixed Model (LMM) analysis using lmer
    # Date is a fixed effect and nest side id is a random intercept effect
    lmm <- lmer(
      as.formula(paste0("beh_", i, formula)),
      data = lmm_df
    )

    # Output ANOVA results using car::Anova for lmer objects
    cat("\nANOVA for", measures[i], ":\n")
    print(car::Anova(lmm))

    # Calculate estimated marginal means (EMMs) and pairwise contrasts
    emmeans_result <- emmeans(lmm, ~contrast)
    pairwise_contrasts <- pairs(emmeans_result, adjust = "sidak")

    # Output pairwise comparisons
    cat("\nPairwise Comparisons for", measures[i], ":\n")
    print(summary(pairwise_contrasts, infer = TRUE))

    # Generate compact letter display (CLD) for significance
    cld_result <- cld(emmeans_result, Letters = letters, adjust = "sidak")
    emm_df <- as.data.frame(emmeans_result)
    emm_df$Letters <- cld_result$.group

    # Define labels, titles, and/or y-values based on the measure or metric
    plot_title <- switch(measures[i], # provide all possible options that measures could be!
      "Aggressive" = "Aggressive Interactions",
      "Avoidant" = "Avoidant Interactions",
      "Neutral" = "Neutral Interactions",
      "Cooperative" = "Cooperative Interactions",
      "cposture" = "C-Posture Interactions",
      "lunge" = "Lunge Interactions",
      "nudge" = "Nudge Interactions",
      "bite" = "Bite Interactions",
      "withdraw" = "Withdraw Interactions",
      "uturn" = "U-Turn Interactions",
      "back" = "Back Interactions",
      "headtobody" = "Head-to-Body Interactions",
      "antennation" = "Antennation Interactions",
      "tandemwalking" = "Tandem Walking Interactions",
      "pass" = "Pass Interactions",
      "headtohead" = "Head-to-Head Interactions",
      "sidebyside" = "Side-by-Side Interactions",
      "attemptedpass" = "Attempted Pass Interactions",
    )

    fig_metric_title <- switch(metric,
      "count" = "emm_interaction_counts",
      "avg_duration" = "emm_interaction_avg_durations",
      "tot_duration" = "emm_interaction_tot_durations"
    )

    fig_measure_title <- switch(measures[i],
      "Aggressive" = "",
      "Avoidant" = "",
      "Neutral" = "",
      "Cooperative" = "",
      "cposture" = "_aggr",
      "lunge" = "_aggr",
      "nudge" = "_aggr",
      "bite" = "_aggr",
      "withdraw" = "_avoi",
      "uturn" = "_avoi",
      "back" = "_avoi",
      "headtobody" = "_neut",
      "antennation" = "_neut",
      "tandemwalking" = "_neut",
      "pass" = "_coop",
      "headtohead" = "_coop",
      "sidebyside" = "_coop",
      "attemptedpass" = "_coop",
    )

    y_label <- switch(metric,
      "count" = "# of Interactions",
      "avg_duration" = "Average Duration (s)",
      "tot_duration" = "Total Duration (s)"
    )

    # Determine the directory and title for the final plot
    fig_dir <- paste0(BASE_DIR_FIGURES, fig_metric_title, fig_measure_title, ".jpeg")

    # Create plot for the current measure
    box_plots <- ggplot(lmm_df, aes(x = contrast, y = !!sym(paste0("beh_", i)), color = "contrast")) +
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
  whole_plot <- wrap_plots(plotlist, ncol = 2)
  ggsave(fig_dir, whole_plot, width = 8, height = 6, dpi = 300)

}

# Function to plot behavioral metrics by caste
plot_by_caste <- function(data_df, metric, plot_dir) {

  measures <- unique(data_df$Behavioral.category)

  # Create a list to store the plots and initialize a counter
  plotlist <- list()
  i <- 1

  for (measure in measures) {
    # Prepare the data: filter missing values, select relevant columns, and factorize the "Caste" variable
    plot_df <- data_df %>%
      filter(Behavioral.category == measure) %>%
      mutate(Caste = factor(Caste, levels = c("queen", "worker", "solitary")))

    # Conduct pairwise t-tests between groups and adjust p-values using the Bonferroni method
    stats_df <- pairwise.t.test(plot_df[[metric]], plot_df$Caste, p.adjust.method = "bonferroni")

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

    # Define labels, titles, and/or y-values based on the measure or metric
    plot_title <- switch(measure,
      "Aggressive" = "Aggressive Interactions",
      "Avoidant" = "Avoidant Interactions",
      "Neutral" = "Neutral Interactions",
      "Tolerant/Cooperative" = "Cooperative Interactions",
      "All Interactions" = "All Interactions"
    )

    fig_title <- switch(metric,
      "count" = "individual_counts_signif.jpeg",
      "avg_duration" = "individual_avg_durations_signif.jpeg",
      "tot_duration" = "individual_tot_durations_signif.jpeg"
    )

    y_label <- switch(metric,
      "count" = "# of Interactions",
      "avg_duration" = "Average Duration (s)",
      "tot_duration" = "Total Duration (s)"
    )

    # Determine the directory and title for the final plot
    fig_dir <- paste0(BASE_DIR_FIGURES, fig_title)

    # Create a plot for number of interactions per caste with significance annotations
    box_plots <- ggplot(plot_df, aes(x = Caste, y = !!sym(metric), fill = Caste)) +
      geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
      geom_jitter(aes(color = Caste), width = 0.15, size = 2, alpha = 0.8) +
      scale_fill_manual(values = CASTE_COLORS) +
      scale_color_manual(values = CASTE_COLORS) +
      labs(
        title = plot_title,
        x = "Caste",
        y = y_label
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
  ggsave(fig_dir, whole_plot, width = 8, height = 6, dpi = 300)
}
