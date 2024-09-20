# example_script.py

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Generate example data
np.random.seed(123)
data = pd.DataFrame(
    {
        "category": np.repeat(["A", "B", "C"], 100),
        "value": np.concatenate(
            [
                np.random.normal(5, 2, 100),
                np.random.normal(7, 2.5, 100),
                np.random.normal(6, 1.5, 100),
            ]
        ),
    }
)

# Print the first few rows of the data
print(data.head())

# Summarize the data
summary_data = (
    data.groupby("category")
    .agg(mean_value=("value", "mean"), sd_value=("value", "std"))
    .reset_index()
)

# Print the summary data
print(summary_data)

# Create a boxplot
plt.figure(figsize=(10, 6))
data.boxplot(column="value", by="category", grid=False)
plt.title("Boxplot of Values by Category")
plt.suptitle("")
plt.xlabel("Category")
plt.ylabel("Value")

# Save the plot
plt.savefig("boxplot.png")
plt.show()
