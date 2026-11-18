"""Per-frame tag brightness of every tracked instance (no smoothing), for scoring trackers.

Uses the NAPS brightness reader (naps.color_tags: percentile of a disc around the tag node,
per-video Otsu threshold, abstention margin). Run with the NAPS env (.venv-naps).

  python tag_colors.py tracked.slp video.mp4 out.csv [--radius 5 --percentile 75 --margin 10]
CSV: frame, track, x, y, value, color (purple = bright dot | black = dark | '' abstain/no tag)
"""

import argparse
import csv

import numpy as np
import sleap_io as sio
from naps.color_tags import classify, measure, otsu_threshold


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("slp")
    ap.add_argument("video")
    ap.add_argument("out")
    ap.add_argument("--node", default="tag")
    ap.add_argument("--radius", type=float, default=5)
    ap.add_argument("--percentile", type=float, default=75)
    ap.add_argument("--margin", type=float, default=10)
    a = ap.parse_args()
    labels = sio.load_slp(a.slp, open_videos=False)
    last = max(lf.frame_idx for lf in labels.labeled_frames)
    r = measure(a.slp, a.video, tag_node=a.node, radius=a.radius, percentile=a.percentile,
                end_frame=last)  # fmt: skip
    thr = otsu_threshold(r.value)
    calls = classify(r.value, thr, a.margin)
    with open(a.out, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["frame", "track", "x", "y", "value", "color"])
        for lf in labels.labeled_frames:
            f = lf.frame_idx
            if f >= len(r.n_inst):
                continue
            for j, inst in enumerate(lf.instances):
                x, y = r.tag_xy[f, j]
                v = r.value[f, j]
                col = {1: "purple", -1: "black"}.get(int(calls[f, j]), "")
                w.writerow([f, inst.track.name if inst.track else "",
                            "" if np.isnan(x) else f"{x:.1f}", "" if np.isnan(y) else f"{y:.1f}",
                            "" if np.isnan(v) else f"{v:.1f}", col])  # fmt: skip
    print(f"{a.slp}: threshold {thr:.1f}; bright {(calls == 1).sum()}, dark {(calls == -1).sum()}")


if __name__ == "__main__":
    main()
