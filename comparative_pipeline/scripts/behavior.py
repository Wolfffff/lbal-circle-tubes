"""Per-video behaviour measures from the cleaned trajectories (same files as lbal-circle-tubes).

From <video>.analysis.h5 (smooth_tracks.py) writes, into --out-dir:
  <video>.interactions.tsv   naps-interactions (Head_to_Head, Head_to_Body; configs/interactions.yaml)
  <video>_hth_bouts.csv      Head_to_Head frames with a Bout id (frames <= --bout-gap apart = one bout)
  <video>_htb_bouts.csv      Head_to_Body, same
  <video>_vels.csv           per-frame speed (px/frame) of --vel-node, one column per bee
  <video>.summary.json       counts + motion fraction per bee

These match the inputs of lbal-circle-tubes/scripts/R (interactions/, batch_degree/*_bouts.csv,
velocities/*_vels.csv; columns ArUcoTag#black / ArUcoTag#purple).

Differences from the original scripts (on purpose):
- bouts are split per (origin, destination) pair; analyze_network.py took frame differences over
  the interleaved rows of both directions.
- velocity: positions forward-filled ("pad") then np.gradient, as culledvelocities.py, but of a
  chosen node (default thorax; the original used node 1 = head) and of both bees.
"""

import argparse
import json
from pathlib import Path

import h5py
import numpy as np
import pandas as pd
from naps.utils.tracking import TrackingArray


def bouts(df, gap):
    out = []
    n = 0
    for (_, o, d), g in df.groupby(["Interaction Name", "Origin interactor",
                                    "Destination interactor"], sort=False):  # fmt: skip
        g = g.sort_values("Interaction Frame")
        b = g["Interaction Frame"].diff().gt(gap).cumsum() + n + 1
        n = int(b.max()) if len(b) else n
        out.append(g.assign(Bout=b.astype(int)))
    cols = list(df.columns) + ["Bout"]
    return (pd.concat(out).sort_values("Interaction Frame") if out
            else pd.DataFrame(columns=cols))  # fmt: skip


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("h5")
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--name", required=True, help="output file prefix (video name)")
    ap.add_argument("--model", required=True, help="interaction model YAML")
    ap.add_argument("--bout-gap", type=int, default=20, help="frames between interaction frames "
                    "that still count as the same bout (lbal-circle-tubes: 20)")  # fmt: skip
    ap.add_argument("--vel-node", default="thorax")
    ap.add_argument("--fps", type=float, default=20.0)
    ap.add_argument("--mm-per-px", type=float, default=None,
                    help="scale for the motion fraction (lbal-circle-tubes: 0.0946)")
    ap.add_argument("--moving-mm-s", type=float, default=1.0)
    ap.add_argument("--skip-frames", type=int, default=200,
                    help="frames excluded from the motion fraction (as the R analysis)")
    ap.add_argument("--threads", type=int, default=1)
    a = ap.parse_args()

    out = Path(a.out_dir)
    out.mkdir(parents=True, exist_ok=True)
    pre = out / a.name

    with h5py.File(a.h5, "r") as f:
        empty = f["tracks"].shape[0] < 2
    if empty:
        Path(f"{pre}.summary.json").write_text(json.dumps({"video": a.name, "status": "no identity"}))
        print(f"{a.name}: fewer than two identified bees; nothing to measure")
        return

    # interactions
    ta = TrackingArray.fromH5(a.h5, model_yaml=a.model, threads=a.threads)
    inter_path = Path(f"{pre}.interactions.tsv")
    ta.assign(str(inter_path), frame_start=0, frame_end=-1)
    inter = pd.read_csv(inter_path, sep="\t") if inter_path.exists() else pd.DataFrame()
    summary = {"video": a.name, "status": "ok"}
    for short, name in [("hth", "Head_to_Head"), ("htb", "Head_to_Body")]:
        df = inter[inter["Interaction Name"] == name] if len(inter) else inter
        b = bouts(df, a.bout_gap) if len(df) else pd.DataFrame(
            columns=list(inter.columns) + ["Bout"])  # fmt: skip
        b.reset_index(drop=True).to_csv(f"{pre}_{short}_bouts.csv")
        summary[f"{short}_frames"] = int(len(df))
        summary[f"{short}_bouts"] = int(b["Bout"].nunique()) if len(b) else 0

    # velocities
    with h5py.File(a.h5, "r") as f:
        tracks = f["tracks"][:]  # (bees, 2, nodes, frames)
        names = [n.decode() for n in f["track_names"][:]]
        nodes = [n.decode() for n in f["node_names"][:]]
    k = nodes.index(a.vel_node)
    vel = {}
    for i, name in enumerate(names):
        xy = pd.DataFrame(tracks[i, :, k, :].T).ffill().to_numpy()
        v = np.linalg.norm(np.stack([np.gradient(xy[:, c]) for c in range(2)], 1), axis=1)
        vel[name] = v
    vdf = pd.DataFrame(vel)
    vdf.to_csv(f"{pre}_vels.csv")
    summary["frames"] = int(len(vdf))
    for name in names:
        v = vdf[name].to_numpy()[a.skip_frames:]
        summary[f"{name}_median_speed_px_s"] = float(np.nanmedian(v) * a.fps)
        if a.mm_per_px:
            summary[f"{name}_moving_frac"] = float(
                np.nansum(v * a.mm_per_px * a.fps >= a.moving_mm_s) / max(len(v), 1))
    Path(f"{pre}.summary.json").write_text(json.dumps(summary, indent=1))
    print(json.dumps(summary))


if __name__ == "__main__":
    main()
