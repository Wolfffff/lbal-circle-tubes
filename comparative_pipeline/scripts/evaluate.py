"""End-to-end top-down evaluation (centroid -> centered-instance) on held-out frames.

  python evaluate.py --centroid models/v3.centroid --instance models/v3.centered_instance \
      --val base=data/split/base.val.slp aurata=... --out results/v3/eval.tsv --sleap-nn .venv035/bin/sleap-nn

Per dataset: detection recall, per-instance mean-node error (median/p90, px), thorax error,
tag recall (GT tag visible -> predicted tag present) and tag error. Tag accuracy is what
NAPS needs: it classifies the color inside a radius-10 circle around the predicted tag.
"""

import argparse
import subprocess
from pathlib import Path

import numpy as np
import sleap_io as sio
from scipy.optimize import linear_sum_assignment


def points_by_frame(labels, user):
    out = {}
    for lf in labels.labeled_frames:
        insts = lf.user_instances if user else lf.predicted_instances
        if insts:
            out[(Path(lf.video.filename).name, lf.frame_idx)] = np.stack([i.numpy() for i in insts])
    return out


def score(gt_path, pr_path, match_px=60.0):
    gt_labels = sio.load_slp(str(gt_path), open_videos=False)
    nodes = gt_labels.skeleton.node_names
    gt = points_by_frame(gt_labels, user=True)
    pr = points_by_frame(sio.load_slp(str(pr_path), open_videos=False), user=False)
    tag, th = nodes.index("tag"), nodes.index("thorax")
    n_gt = n_match = tag_gt = tag_hit = 0
    err, th_err, tag_err = [], [], []
    for key, G in gt.items():
        n_gt += len(G)
        tag_gt += int((~np.isnan(G[:, tag, 0])).sum())
        P = pr.get(key)
        if P is None:
            continue
        D = np.linalg.norm(G[:, None] - P[None], axis=-1)          # gt x pred x node
        with np.errstate(all="ignore"):
            C = np.nanmean(D, axis=-1)
        r, c = linear_sum_assignment(np.nan_to_num(C, nan=1e6))
        for i, j in zip(r, c):
            if not C[i, j] <= match_px:
                continue
            n_match += 1
            err.append(C[i, j])
            if not np.isnan(D[i, j, th]):
                th_err.append(D[i, j, th])
            if not np.isnan(G[i, tag, 0]) and not np.isnan(P[j, tag, 0]):
                tag_hit += 1
                tag_err.append(D[i, j, tag])
    q = lambda x, p: float(np.percentile(x, p)) if x else float("nan")
    return {"frames": len(gt), "instances": n_gt, "recall": n_match / max(n_gt, 1),
            "err_med": q(err, 50), "err_p90": q(err, 90), "thorax_med": q(th_err, 50),
            "tag_recall": tag_hit / max(tag_gt, 1), "tag_med": q(tag_err, 50),
            "tag_p90": q(tag_err, 90), "tag_within10": float(np.mean(np.array(tag_err) <= 10))
            if tag_err else float("nan")}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--centroid", required=True)
    ap.add_argument("--instance", required=True)
    ap.add_argument("--val", nargs="+", required=True, help="name=path.slp")
    ap.add_argument("--out", required=True)
    ap.add_argument("--sleap-nn", default="sleap-nn")
    ap.add_argument("--peak-threshold", default="0.2")
    a = ap.parse_args()
    out = Path(a.out)
    pred_dir = out.parent / "eval_predictions"
    pred_dir.mkdir(parents=True, exist_ok=True)
    rows = []
    for item in a.val:
        name, gt = item.split("=", 1)
        pr = pred_dir / f"{name}.slp"
        subprocess.run([a.sleap_nn, "predict", "-m", a.centroid, "-m", a.instance, "-i", gt,
                        "-o", str(pr), "--only_labeled_frames", "-n", "2",
                        "--peak_threshold", a.peak_threshold], check=True,
                       stdout=subprocess.DEVNULL)
        rows.append({"dataset": name, **score(gt, pr)})
    cols = list(rows[0])
    lines = ["\t".join(cols)] + ["\t".join(f"{r[c]:.3f}" if isinstance(r[c], float) else str(r[c])
                                           for c in cols) for r in rows]
    out.write_text("\n".join(lines) + "\n")
    print("\n".join(lines))


if __name__ == "__main__":
    main()
