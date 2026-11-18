"""Score tracker outputs against per-frame tag brightness (from tag_colors.py).

No ground-truth identities exist, but exactly one bee per arena carries a bright ("purple") dot,
and a bright reading is reliable (a dark reading may be a hidden bright dot). A good tracker keeps
the bright dot on one track; an identity swap shows up as the bright dot moving to the other track
and staying there.

  python score_tracking.py --out summary.tsv --inputs tracker/video=colors.csv ...

Per (tracker, video):
  tracks           number of distinct track ids (2 is ideal)
  coverage         fraction of instances that carry a track id
  bright_frac      fraction of instances with a bright reading
  purity           fraction of bright readings on the track that carries most of them, computed
                   per segment between swaps (i.e. isolated misreads)
  switches_per_10k sustained moves of the bright dot to another track per 10k frames (majority
                   filter over --half readings on each side)
"""

import argparse
from collections import Counter

import numpy as np
import pandas as pd


def majority_filter(seq, half):
    c = np.asarray(seq)
    return [Counter(c[max(0, i - half): i + half + 1]).most_common(1)[0][0] for i in range(len(c))]


def score(csv_path, half=10):
    df = pd.read_csv(csv_path, keep_default_na=False)
    n_frames = df["frame"].nunique()
    tracked = df[df["track"] != ""]
    bright = tracked[tracked["color"] == "purple"].sort_values("frame")
    seq = bright["track"].tolist()
    sw, agree = 0, 0
    if len(seq) >= 2 * half + 1:
        sm = majority_filter(seq, half)
        sw = int(sum(a != b for a, b in zip(sm[:-1], sm[1:])))
        agree = int(sum(a == b for a, b in zip(seq, sm)))
    elif seq:
        agree = Counter(seq).most_common(1)[0][1]
    return {"frames": n_frames,
            "inst_per_frame": len(df) / max(n_frames, 1),
            "tracks": tracked["track"].nunique(),
            "coverage": len(tracked) / max(len(df), 1),
            "bright_frac": len(bright) / max(len(df), 1),
            "purity": agree / max(len(seq), 1),
            "switches_per_10k": 1e4 * sw / max(n_frames, 1)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--inputs", nargs="+", required=True, help="tracker/video=colors.csv")
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    rows = []
    for item in a.inputs:
        key, path = item.split("=", 1)
        tracker, video = key.split("/", 1)
        rows.append({"tracker": tracker, "video": video, **score(path)})
    df = pd.DataFrame(rows)
    mean = df.drop(columns="video").groupby("tracker").mean(numeric_only=True)
    mean = mean.sort_values(["switches_per_10k", "purity"], ascending=[True, False])
    with open(a.out, "w") as fh:
        fh.write("# mean over videos (best first)\n")
        mean.round(4).to_csv(fh, sep="\t")
        fh.write("\n# per video\n")
        df.round(4).to_csv(fh, sep="\t", index=False)
    print(mean.round(4).to_string())


if __name__ == "__main__":
    main()
