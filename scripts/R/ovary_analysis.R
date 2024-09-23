# Load constants and utility functions
source("scripts/R/constants.R")
source("scripts/R/utils.R")

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

# Prepare the data: filter missing values, select relevant columns, and factorize the "Caste" variable
ovary_plot_df <- master_data %>%
  filter(!is.na(Ovarian.Index)) %>%
  select(Caste, Ovarian.Index) %>%
  mutate(Caste = factor(Caste, levels = c("queen", "worker", "solitary")))

# Conduct pairwise t-tests between groups and adjust p-values using the Bonferroni method
stats_df <- ovary_plot_df %>%
  pairwise_t_test(Ovarian.Index ~ Caste, p.adjust.method = "bonferroni")

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
ovary_plot <- ggplot(ovary_plot_df, aes(x = Caste, y = Ovarian.Index, fill = Caste)) +
  geom_boxplot(alpha = 0.5, outlier.shape = NA, width = 0.6) +
  geom_jitter(aes(color = Caste), width = 0.15, size = 2, alpha = 0.8) +
  scale_fill_manual(values = CASTE_COLORS) +
  scale_color_manual(values = CASTE_COLORS) +
  labs(
    title = "Ovarian Development per Caste",
    x = "Caste",
    y = "Ovary Development Index"
  ) +
  stat_compare_means(
    method = "t.test",
    label = "p.signif",
    comparisons = comparisons_list,
    hide.ns = TRUE,
    step.increase = 0.1
  ) +
  SHARED_THEME

# Save the plot to the specified directory
ggsave("figures/ovarian_development_signif.jpeg", ovary_plot, width = 8, height = 6, dpi = 300)
