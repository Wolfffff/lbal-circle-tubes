import sleap
import numpy as np
import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt
import seaborn as sns
import os

# load the names of models ending in ".centered_instance"
subfolders = [f.path for f in os.scandir("models") if f.is_dir() and f.name.endswith(".centered_instance")]

# delete subfolders that do not contain a metrics files
subfolders_with_metrics = []
for s in subfolders:
    if os.path.exists(s + "/metrics.val.npz"):
        subfolders_with_metrics.append(s)
print(subfolders_with_metrics)

# load the metrics for the validation set
avg_dist = []
oks_map = []
for s in subfolders_with_metrics:
    metrics = sleap.load_metrics(s, split = "val")
    
    avg_dist.append(metrics["dist.avg"])
    oks_map.append(metrics["oks_voc.mAP"])

filtered_data = [(dist, oks) for dist, oks in zip(avg_dist, oks_map) if oks != 0]
filtered_avg_dist, filtered_oks_map = zip(*filtered_data)

# plot OKS mAP versus average distance error
fig, ax = plt.subplots(figsize = (6,6))
ax.scatter(filtered_avg_dist, filtered_oks_map)
ax.set_xlabel("Average distance error (pixels)")
ax.set_ylabel("OKS mAP")
ax.set_title("OKS mAP versus average distance error")

# draw a circle around the point with the best OKS mAP (this is kinda wonky)
best_oks_map = max(oks_map)
best_avg_dist = avg_dist[oks_map.index(best_oks_map)]
circle = plt.Circle((best_avg_dist, best_oks_map), 0.5, color = "r", fill = False)
ax.add_patch(circle)

ax.set_xlim([min(filtered_avg_dist) - 1, max(filtered_avg_dist) + 1])
ax.set_ylim([-0.1, 1.1])

ax.set_box_aspect(1)
plt.show()

# save figure as a .png file
fig.savefig("model_evaluation.png")

# print the model name with the best OKS mAP
best_model = subfolders_with_metrics[oks_map.index(best_oks_map)]
print(best_model)

