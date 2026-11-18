"""Fixed, per-dataset train/val split of user-labeled frames.

Split is by frame (both bees in a frame go to the same side), stratified per dataset,
deterministic given the seed. Writes <out>/<name>.train.slp and <out>/<name>.val.slp.

  python split_labels.py --out data/split --seed 0 --val name=frac ... --inputs name=path ...
"""

import argparse
import random
from pathlib import Path

import sleap_io as sio


def kv(items, cast=str):
    return {k: cast(v) for k, v in (i.split("=", 1) for i in items)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--inputs", nargs="+", required=True)
    ap.add_argument("--val", nargs="+", required=True, help="name=fraction")
    ap.add_argument("--out", required=True)
    ap.add_argument("--seed", type=int, default=0)
    a = ap.parse_args()
    inputs, val = kv(a.inputs), kv(a.val, float)
    out = Path(a.out)
    out.mkdir(parents=True, exist_ok=True)
    for name, path in inputs.items():
        labels = sio.load_slp(path, open_videos=False)
        frames = [lf for lf in labels.labeled_frames if lf.user_instances]
        for lf in frames:  # keep ground truth only, drop track identities
            lf.instances = list(lf.user_instances)
            for inst in lf.instances:
                inst.track = None
        rng = random.Random(f"{a.seed}:{name}")
        idx = list(range(len(frames)))
        rng.shuffle(idx)
        n_val = max(1, round(val[name] * len(frames)))
        parts = {"val": sorted(idx[:n_val]), "train": sorted(idx[n_val:])}
        for part, ids in parts.items():
            sub = sio.Labels(labeled_frames=[frames[i] for i in ids], videos=labels.videos,
                             skeletons=labels.skeletons, tracks=[])
            sio.save_slp(sub, out / f"{name}.{part}.slp")
            n = sum(len(lf.instances) for lf in sub.labeled_frames)
            print(f"{name:11s} {part:5s} {len(ids):5d} frames {n:5d} instances")


if __name__ == "__main__":
    main()
