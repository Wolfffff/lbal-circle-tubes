# Load necessary libraries
library(ggplot2)
library(dplyr)

# Generate example data
set.seed(123)
data <- data.frame(
  category = rep(c("A", "B", "C"), each = 100),
  value = c(rnorm(100, mean = 5, sd = 2), 
            rnorm(100, mean = 7, sd = 2.5), 
            rnorm(100, mean = 6, sd = 1.5))
)

# Print the first few rows of the data
head(data)

# Summarize the data
summary_data <- data %>%
  group_by(category) %>%
  summarize(
    mean_value = mean(value),
    sd_value = sd(value)
  )

# Print the summary data
print(summary_data)

# Create a boxplot
ggplot(data, aes(x = category, y = value, fill = category)) +
  geom_boxplot() +
  labs(title = "Boxplot of Values by Category",
       x = "Category",
       y = "Value") +
  theme_minimal()

# Save the plot
ggsave("figures/boxplot.png")
