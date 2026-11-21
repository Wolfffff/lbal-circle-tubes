"""Collect per-video behavior summaries into one table (+ immobile_bees: bees moving in < 5 % of frames).

  python behavior_table.py --out behavior.tsv results/v3/<ds>/behavior/*.summary.json
"""
import argparse
import json

import pandas as pd

IMMOBILE = 0.05  # moving in < 5 % of frames (median bee: 46 %)

ap = argparse.ArgumentParser()
ap.add_argument("summaries", nargs="*")
ap.add_argument("--out", required=True)
a = ap.parse_args()
rows = [json.load(open(f)) for f in sorted(a.summaries)]
df = pd.DataFrame(rows)
# flag bees that (almost) never move: dead, torpid or stuck -> check before analysis
frac_cols = [c for c in df.columns if c.endswith("_moving_frac")]
df["immobile_bees"] = [
    ",".join(c.split("#")[1].replace("_moving_frac", "") for c in frac_cols
             if pd.notna(r[c]) and r[c] < IMMOBILE)
    for _, r in df.iterrows()
] if frac_cols else ""
df.to_csv(a.out, sep="\t", index=False)
flagged = df[df["immobile_bees"].astype(str).str.len() > 0] if frac_cols else df.iloc[:0]
for _, r in flagged.iterrows():
    print(f"immobile: {r.video}: {r.immobile_bees}")
print(f"{len(rows)} videos -> {a.out}")
