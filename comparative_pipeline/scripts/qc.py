"""Per-dataset QC table for the tracking pipeline outputs.

  python qc.py --out qc.tsv --videos v1 v2 ... --tracked DIR --naps DIR --filtered DIR

Columns per video: frames with detections, instances/frame, fraction of frames with exactly
2 bees, thorax/tag found rate, mean instance score, SLEAP tracks, NAPS identity coverage
(fraction of instances assigned black/purple), and whether the filtered file exists.
"""

import argparse
from pathlib import Path

import numpy as np
import pandas as pd
import sleap_io as sio


def summarize(video, tracked, naps, filtered):
    r = {"video": video}
    L = sio.load_slp(str(tracked), open_videos=False)
    nodes = L.skeleton.node_names
    P, scores, per_frame = [], [], []
    for lf in L.labeled_frames:
        per_frame.append(len(lf.instances))
        for inst in lf.instances:
            P.append(inst.numpy())
            scores.append(getattr(inst, "score", np.nan))
    P = np.array(P)
    n = max(len(per_frame), 1)
    r.update(frames=len(per_frame), inst_per_frame=float(np.mean(per_frame)) if per_frame else 0,
             two_bees=float(np.mean(np.array(per_frame) == 2)) if per_frame else 0,
             thorax=float((~np.isnan(P[:, nodes.index("thorax"), 0])).mean()) if len(P) else 0,
             tag=float((~np.isnan(P[:, nodes.index("tag"), 0])).mean()) if len(P) else 0,
             score=float(np.nanmean(scores)) if scores else np.nan,
             tracks=len({i.track.name for lf in L.labeled_frames for i in lf.instances if i.track}))
    if Path(naps).exists():
        N = sio.load_slp(str(naps), open_videos=False)
        names = [i.track.name if i.track else "" for lf in N.labeled_frames for i in lf.instances]
        ok = [n for n in names if n and "none" not in n.lower()]
        r["naps_id_coverage"] = len(ok) / max(len(names), 1)
        r["naps_ids"] = len(set(ok))
    r["filtered"] = Path(filtered).exists()
    return r


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--videos", nargs="+", required=True)
    ap.add_argument("--tracked", required=True)
    ap.add_argument("--naps", required=True)
    ap.add_argument("--filtered", required=True)
    ap.add_argument("--out", required=True)
    a = ap.parse_args()
    rows = [summarize(v, Path(a.tracked) / f"{v}.slp", Path(a.naps) / f"{v}.slp",
                      Path(a.filtered) / f"{v}.slp") for v in a.videos]
    df = pd.DataFrame(rows)
    df.round(4).to_csv(a.out, sep="\t", index=False)
    print(df.describe().round(3).to_string())


if __name__ == "__main__":
    main()
