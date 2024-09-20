# plots.R

# Function to create boxplots for ovarian development per caste
plot_ovarian_development <- function(plot_df, colors) {
  ggplot(data = plot_df) +
    geom_boxplot(aes(x = 1, y = queens, fill = "queens"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
    geom_boxplot(aes(x = 2, y = workers, fill = "workers"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
    geom_boxplot(aes(x = 3, y = solitary_reproductives, fill = "solitary_reproductives"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
    geom_jitter(aes(x = 1, y = queens, colour = "queens"), alpha = 0.5, width = 0.2) +
    geom_jitter(aes(x = 2, y = workers, colour = "workers"), alpha = 0.5, width = 0.2) +
    geom_jitter(aes(x = 3, y = solitary_reproductives, colour = "solitary_reproductives"), alpha = 0.5, width = 0.2) +
    scale_x_continuous(breaks = c(1, 2, 3), labels = c("queens", "workers", "solitary_reproductives")) +
    scale_color_manual(name = "Point Legend", values = colors) +
    scale_fill_manual(name = "Boxplot Legend", values = colors) +
    labs(x = "Caste", y = "Body-Size Corrected Ovary Development Index") +
    theme_classic()
}

# Plot for motion and interactions
plot_motion_interactions <- function(plot_df, colors) {
  ggplot(data = plot_df) +
    geom_boxplot(aes(x = factor(contrast), y = motion_prop * 100, fill = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
    geom_boxplot(aes(x = factor(contrast), y = hth_bouts, fill = "total head-to-head interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
    geom_boxplot(aes(x = factor(contrast), y = htb_bouts, fill = "total head-to-body interactions"), alpha = 0.5, width = 0.4, outlier.shape = NA) +
    geom_jitter(aes(x = factor(contrast), y = motion_prop * 100, colour = "proportion of time spent moving (%)"), alpha = 0.5, width = 0.2) +
    geom_jitter(aes(x = factor(contrast), y = hth_bouts, colour = "total head-to-head interactions"), alpha = 0.5, width = 0.2) +
    geom_jitter(aes(x = factor(contrast), y = htb_bouts, colour = "total head-to-body interactions"), alpha = 0.5, width = 0.2) +
    scale_color_manual(name = "Point Legend", values = colors) +
    scale_fill_manual(name = "Boxplot Legend", values = colors) +
    theme_classic() +
    labs(x = "Social Contrast", y = "")
}

# Save plots
save_plot <- function(filename) {
  ggsave(filename, width = 10, height = 5)
}
