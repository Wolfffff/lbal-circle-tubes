"""Held-out identity benchmark for one tracked video (brightness colour tags).

1. naps-color-decodes on the tracked .slp -> all bright/dark reads.
2. Hold out every read in random blocks of --block frames covering --holdout of the frames.
3. Run each NAPS variant on the remaining reads (the old color_track NAPS uses its own classifier
   and never sees the reads, so for it every read is held out).
4. Score every variant on the held-out frames: an instance with a held-out *bright* read must
   carry the bright tag and one with a held-out *dark* read (the partner of a bright read in the
   same frame) the dark tag.

Reports per variant: held-out accuracy (bright reads; bright+dark pairs), coverage (labelled
instances / all instances), identity jumps per 10k frames, runtime.

  python identity_bench.py --slp tracked.slp --video v.mp4 --out-dir dir [--naps-bin .venv-naps/bin]
"""

import argparse
import json
import subprocess
import sys
import time
from pathlib import Path

import numpy as np
import sleap_io as sio

HERE = Path(__file__).resolve().parent


def run(cmd, log):
    t = time.time()
    with open(log, "a") as fh:
        fh.write("$ " + " ".join(map(str, cmd)) + "\n")
        fh.flush()
        r = subprocess.run(list(map(str, cmd)), stdout=fh, stderr=subprocess.STDOUT)
    if r.returncode:
        raise RuntimeError(f"failed ({r.returncode}): {' '.join(map(str, cmd))}; see {log}")
    return time.time() - t


def identities(path, names, n_frames):
    """(F, S) array of tag ids per (frame, instance slot) in the slot order of the input slp.

    NAPS writes instances in the same order as the input; the old color_track may reorder, so
    instances are matched to the input by tag/thorax position instead of slot.
    """
    labels = sio.load_slp(str(path), open_videos=False)
    out = {}
    for lf in labels.labeled_frames:
        rows = []
        for inst in lf.instances:
            name = inst.track.name if inst.track else None
            tag = None
            if name and name.startswith("ArUcoTag#"):
                tag = names.get(name.split("#", 1)[1])
            rows.append((tag, inst.numpy()))
        out[lf.frame_idx] = rows
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--slp", required=True)
    ap.add_argument("--video", required=True)
    ap.add_argument("--out-dir", required=True)
    ap.add_argument("--naps-bin", default=str(HERE.parent / ".venv-naps/bin"))
    ap.add_argument("--old-python", help="python with sleap 1.3 for the old color_track NAPS")
    ap.add_argument("--holdout", type=float, default=0.2)
    ap.add_argument("--block", type=int, default=20)
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--jump", type=float, default=60.0)
    a = ap.parse_args()

    out = Path(a.out_dir)
    out.mkdir(parents=True, exist_ok=True)
    log = out / "bench.log"
    log.write_text("")
    nb = Path(a.naps_bin)
    full = out / "reads.npz"
    run([nb / "naps-color-decodes", "--slp-path", a.slp, "--video-path", a.video,
         "--output-path", full, "--dark-votes"], log)  # fmt: skip
    d = dict(np.load(full))
    n_frames = len(d["frame_idx"])

    # held-out blocks
    rng = np.random.default_rng(a.seed)
    n_blocks = n_frames // a.block
    held_blocks = rng.choice(n_blocks, int(round(a.holdout * n_blocks)), replace=False)
    held = np.zeros(n_frames, bool)
    for b in held_blocks:
        held[b * a.block : (b + 1) * a.block] = True
    vf, vi, vt = d["vote_frame"], d["vote_inst"], d["vote_tag"]
    bright = vt == 1

    def write(path, keep):
        dd = dict(d)
        for k in ("vote_frame", "vote_inst", "vote_tag", "vote_conf"):
            dd[k] = d[k][keep]
        np.savez_compressed(path, **dd)

    train = ~held[vf]
    write(out / "train_bright.npz", train & bright)
    write(out / "train_both.npz", train)

    # truth on held-out frames: bright reads, plus dark reads whose frame-partner reads bright
    truth = {}
    for f, j, t in zip(vf[held[vf]], vi[held[vf]], vt[held[vf]]):
        truth.setdefault(int(f), {})[int(j)] = int(t)
    truth_bright = {(f, j) for f, r in truth.items() for j, t in r.items() if t == 1}
    truth_pair = {(f, j): t for f, r in truth.items() if sorted(r.values()) == [0, 1]
                  for j, t in r.items()}  # fmt: skip

    common = ["--slp-path", a.slp, "--video-path", a.video, "--tag-node-name", "tag",
              "--start-frame", 0, "--end-frame", n_frames - 1,
              "--aruco-marker-set", "DICT_4X4_50", "--closed-set", 0, 1]  # fmt: skip
    joint = ["--identity-engine", "joint", "--tracklets", "built", "--colony-size", 2]
    variants = {
        "joint_0.99": (["--decodes", out / "train_bright.npz", *joint, "--min-confidence", 0.99]),
        "joint_0.95": (["--decodes", out / "train_bright.npz", *joint, "--min-confidence", 0.95]),
        "joint_0.98_emit": ([
            "--decodes", out / "train_bright.npz", *joint, "--min-confidence", 0.98,
            "--untrusted-policy", "emit"]),  # fmt: skip
        "joint_both_0.99": (["--decodes", out / "train_both.npz", *joint]),
        "joint_sleaptracks": ([
            "--decodes", out / "train_bright.npz", "--identity-engine", "joint",
            "--tracklets", "sleap", "--colony-size", 2]),  # fmt: skip
        "legacy_w50": (["--decodes", out / "train_bright.npz", "--half-rolling-window-size", 50]),
        "legacy_w200": (["--decodes", out / "train_bright.npz", "--half-rolling-window-size", 200]),
        "legacy_both_w200": ([
            "--decodes", out / "train_both.npz", "--half-rolling-window-size", 200]),  # fmt: skip
    }
    results = []
    names_new = {"0": 0, "1": 1}
    runs = []
    for name, extra in variants.items():
        o = out / f"{name}.slp"
        try:
            secs = run([nb / "naps-track", *common, *extra, "--output-path", o, "-q"], log)
            runs.append((name, o, names_new, secs))
        except RuntimeError as e:
            print(e, file=sys.stderr)
    last_tracked = max(lf.frame_idx for lf in sio.load_slp(a.slp, open_videos=False))
    if a.old_python:
        # color_track.py crashes on instances without a track: give it a copy without them
        old_in = out / "old_input.slp"
        lab = sio.load_slp(a.slp, open_videos=False)
        for lf in lab.labeled_frames:
            lf.instances = [i for i in lf.instances if i.track is not None]
        lab.save(str(old_in), restore_original_videos=False)
        o = out / "old_color_track.slp"
        try:
            secs = run([a.old_python, HERE / "legacy" / "color_track.py", "--slp-path", old_in,
                        "--video-path", a.video, "--tag-node-name", "tag", "--start-frame", 0,
                        "--end-frame", last_tracked, "--half-rolling-window-size", 200,
                        "--tag-crop-type", "circle", "--tag-crop-size", 3, "--output-path", o],
                       log)  # fmt: skip
            runs.append(("old_color_track_r3_w200", o, {"black": 0, "purple": 1}, secs))
        except RuntimeError as e:
            print(e, file=sys.stderr)

    src = sio.load_slp(a.slp, open_videos=False)
    tag_k = src.skeleton.node_names.index("tag")
    th_k = src.skeleton.node_names.index("thorax")
    src_xy = {lf.frame_idx: [i.numpy() for i in lf.instances] for lf in src.labeled_frames}
    n_inst = sum(len(v) for v in src_xy.values())

    for name, path, names, secs in runs:
        ids = identities(path, names, n_frames)
        # map output instances to source slots by thorax position
        lab = {}
        for f, rows in ids.items():
            for tag, pts in rows:
                for j, sp in enumerate(src_xy.get(f, [])):
                    a_, b_ = pts[th_k], sp[th_k]
                    if np.all(np.isfinite(a_)) and np.all(np.isfinite(b_)) and np.hypot(*(a_ - b_)) < 1e-2:
                        lab[(f, j)] = tag
                        break
        def acc(truth_map):
            got = [(lab.get(k), t) for k, t in truth_map.items()]
            labelled = [(g, t) for g, t in got if g is not None]
            return (np.mean([g == t for g, t in labelled]) if labelled else float("nan"),
                    len(labelled) / max(len(got), 1))  # fmt: skip
        acc_b, cov_b = acc({k: 1 for k in truth_bright})
        acc_p, cov_p = acc(truth_pair)
        coverage = sum(1 for v in lab.values() if v is not None) / max(n_inst, 1)
        jumps, last = 0, {}
        for f in sorted(ids):
            for tag, pts in ids[f]:
                p = pts[th_k]
                if tag is None or not np.all(np.isfinite(p)):
                    continue
                if tag in last and last[tag][0] == f - 1 and np.hypot(*(p - last[tag][1])) > a.jump:
                    jumps += 1
                last[tag] = (f, p)
        results.append({
            "variant": name, "heldout_bright_acc": round(float(acc_b), 4),
            "heldout_bright_cov": round(cov_b, 4), "heldout_pair_acc": round(float(acc_p), 4),
            "heldout_pair_cov": round(cov_p, 4), "coverage": round(coverage, 4),
            "jumps_per_10k": round(jumps / n_frames * 1e4, 1), "seconds": round(secs, 1),
            "n_heldout_bright": len(truth_bright), "n_heldout_pair": len(truth_pair),
        })  # fmt: skip
    cols = list(results[0]) if results else []
    with open(out / "summary.tsv", "w") as fh:
        fh.write("\t".join(cols) + "\n")
        for r in results:
            fh.write("\t".join(str(r[c]) for c in cols) + "\n")
    (out / "summary.json").write_text(json.dumps(results, indent=1))
    print(open(out / "summary.tsv").read())


if __name__ == "__main__":
    main()
