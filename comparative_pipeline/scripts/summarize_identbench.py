"""Combine per-video identity_bench summaries: mean over videos per variant + per-video rows."""
import argparse

import pandas as pd

ap = argparse.ArgumentParser()
ap.add_argument("inputs", nargs="+", help="ds/video=summary.tsv")
ap.add_argument("--out", required=True)
a = ap.parse_args()
df = pd.concat([pd.read_csv(p, sep="\t").assign(video=k) for k, p in (s.split("=", 1) for s in a.inputs)])
cols = ["heldout_bright_acc", "heldout_bright_cov", "heldout_pair_acc", "heldout_pair_cov",
        "coverage", "jumps_per_10k", "seconds"]
mean = df.groupby("variant")[cols].mean().sort_values(["heldout_bright_acc", "coverage"], ascending=False)
with open(a.out, "w") as fh:
    fh.write("# mean over videos (held-out = 20% of tag readings withheld in 1-s blocks)\n")
    mean.round(4).to_csv(fh, sep="\t")
    fh.write("\n# per video\n")
    df.round(4).to_csv(fh, sep="\t", index=False)
print(mean.round(4).to_string())
