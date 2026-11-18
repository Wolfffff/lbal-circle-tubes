"""Leave-one-species-out table: each species scored by the model that never saw it vs the model
trained on everything (same held-out val frames), plus the unseen model on all its labelled frames."""
import argparse

import pandas as pd

ap = argparse.ArgumentParser()
ap.add_argument("inputs", nargs="+", help="ds=results/<model>_loso_<ds>/eval.tsv")
ap.add_argument("--ref", required=True)
ap.add_argument("--out", required=True)
a = ap.parse_args()
ref = pd.read_csv(a.ref, sep="\t").set_index("dataset")
rows = []
for item in a.inputs:
    ds, path = item.split("=", 1)
    e = pd.read_csv(path, sep="\t").set_index("dataset")
    for label, src, key in [("all species (ref)", ref, ds), ("unseen species: val", e, ds),
                            ("unseen species: all labels", e, f"{ds}_all"),
                            ("unseen: base val", e, "base")]:
        if key in src.index:
            rows.append({"species": ds, "model": label, **src.loc[key].to_dict()})
df = pd.DataFrame(rows)
df.round(3).to_csv(a.out, sep="\t", index=False)
print(df.round(3).to_string(index=False))
