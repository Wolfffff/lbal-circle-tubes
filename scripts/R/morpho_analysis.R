# Load constants and utility functions
source("constants.R")
source("utils.R")

# Install and load required packages
install_if_missing(REQUIRED_PACKAGES)
invisible(lapply(REQUIRED_PACKAGES, library, character.only = TRUE))

# Load master data
master_data <- load_master_data(BASE_DIR_MASTER_DATA)

# Analysis of ovary dissections with appropriate statistical tests
# Load additional packages
library(rstatix)
library(ggpubr)
library(ggplot2)
library(dplyr)

measures <- c("Ovarian.Index", "Head.Width..mm.", "IT.Span..mm.")

# Create a list to store the plots and initialize a counter
plotlist <- list()
i <- 1

for (measure in measures) {
  # Prepare the data: filter missing values, select relevant columns, and factorize the "Caste" variable
  plot_df <- master_data %>%
    mutate(!!measure := as.numeric(.data[[measure]])) %>%
    filter(!is.na(.data[[measure]])) %>%
    select(Caste, all_of(measure)) %>%
    mutate(Caste = factor(Caste, levels = c("queen", "worker", "solitary")))

  # Conduct pairwise t-tests between groups and adjust p-values using the Bonferroni method
  stats_df <- plot_df %>%
    pairwise_t_test(formula = as.formula(paste(measure, "~ Caste")), p.adjust.method = "bonferroni")

  # Filter out non-significant comparisons and select only significant group pairs
  significant_comparisons <- stats_df %>%
    filter(p.adj <= 0.05) %>%
    select(group1, group2)

  # Convert the filtered comparisons into a list format suitable for stat_compare_means
  comparisons_list <- lapply(
    split(significant_comparisons, seq.int(nrow(significant_comparisons))),
    function(x) as.character(unlist(x))
  )

  # Create a plot for ovarian development per caste with significance annotations
  morpho_plot <- ggplot(plot_df, aes(x = Caste, y = .data[[measure]], fill = Caste)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
    geom_jitter(aes(color = Caste), width = 0.15, size = 2, alpha = 0.8) +
    scale_fill_manual(values = CASTE_COLORS) +
    scale_color_manual(values = CASTE_COLORS) +
    labs(
      title = paste0(measure, " per Caste"),
      x = "Caste",
      y = measure
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
  plotlist[[i]] <- morpho_plot
  i <- i + 1
}

# Save the plot to the specified directory
whole_plot <- (plotlist[[1]] | plotlist[[2]]) / (plotlist[[3]] | plot_spacer())
ggsave("../../figures/morphological_metrics_signif.jpeg", whole_plot, width = 8, height = 6, dpi = 300)
