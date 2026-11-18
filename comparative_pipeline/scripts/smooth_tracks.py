"""Clean and smooth NAPS-identified trajectories (keeps the NAPS identities).

Replaces filter_nodes.py, which re-assigned identities by angular continuity around the arena
centre (its first frame named the bees by angle order, so black/purple were arbitrary per video).
Here identity comes only from NAPS; per identity track and per smoothed node:

  1. instances without an identity are dropped (NAPS abstained)
  2. spikes: a point farther than --max-jump px from both its neighbours is removed
  3. gaps of <= --max-gap frames are linearly interpolated; longer gaps stay empty
  4. NaN-aware running median over --window frames

Output: one instance per identity per frame, full skeleton (unsmoothed nodes keep their predicted
positions), same track names (ArUcoTag#black / ArUcoTag#purple).

  python smooth_tracks.py naps.slp out.slp [--nodes tag | --nodes all] [--analysis-h5 out.h5]
"""

import argparse

import numpy as np
import sleap_io as sio


def despike(xy, max_jump):
    """Remove points far from both neighbours (single-frame outliers)."""
    out = xy.copy()
    ok = np.isfinite(out).all(1)
    d_prev = np.full(len(out), np.inf)
    d_next = np.full(len(out), np.inf)
    d = np.linalg.norm(np.diff(out, axis=0), axis=1)
    d_prev[1:] = np.where(np.isfinite(d), d, np.inf)
    d_next[:-1] = np.where(np.isfinite(d), d, np.inf)
    spike = ok & (d_prev > max_jump) & (d_next > max_jump)
    # an isolated point (no neighbours at all) is not a spike
    spike &= np.isfinite(np.r_[np.inf, d]) | np.isfinite(np.r_[d, np.inf])
    out[spike] = np.nan
    return out, int(spike.sum())


def interpolate(xy, max_gap):
    out = xy.copy()
    ok = np.isfinite(out).all(1)
    if ok.sum() < 2:
        return out, 0
    idx = np.flatnonzero(ok)
    filled = 0
    for a, b in zip(idx[:-1], idx[1:]):
        gap = b - a - 1
        if 0 < gap <= max_gap:
            t = np.arange(1, gap + 1) / (gap + 1)
            out[a + 1 : b] = out[a] + t[:, None] * (out[b] - out[a])
            filled += gap
    return out, filled


def running_median(xy, window):
    if window <= 1:
        return xy
    h = window // 2
    pad = np.pad(xy, ((h, h), (0, 0)), constant_values=np.nan)
    stack = np.lib.stride_tricks.sliding_window_view(pad, window, axis=0)  # (T, 2, window)
    with np.errstate(all="ignore"):
        import warnings

        with warnings.catch_warnings():
            warnings.simplefilter("ignore", RuntimeWarning)
            med = np.nanmedian(stack, axis=2)
    med[~np.isfinite(xy).all(1)] = np.nan  # never fill a missing point by smoothing
    return med


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("input")
    ap.add_argument("output")
    ap.add_argument("--nodes", nargs="+", default=["tag"], help="nodes to clean/smooth, or 'all'")
    ap.add_argument("--max-jump", type=float, default=60.0, help="spike threshold (px/frame)")
    ap.add_argument("--max-gap", type=int, default=10, help="longest gap to interpolate (frames)")
    ap.add_argument("--window", type=int, default=5, help="running-median window (frames)")
    ap.add_argument("--analysis-h5", help="also write a SLEAP analysis .h5 (tracks x nodes x frames)")
    a = ap.parse_args()

    labels = sio.load_slp(a.input, open_videos=False)
    sk = labels.skeleton
    names = sk.node_names
    nodes = names if a.nodes == ["all"] else a.nodes
    k_nodes = [names.index(n) for n in nodes]
    # sorted: ArUcoTag#black before ArUcoTag#purple (the R analysis reads columns by position)
    tracks = sorted((t for t in labels.tracks if t.name.startswith("ArUcoTag#")), key=lambda t: t.name)
    lfs = sorted(labels.labeled_frames, key=lambda lf: lf.frame_idx)
    if not lfs or not tracks:
        # no identity anywhere (e.g. no detections): write empty outputs so the batch goes on;
        # behavior.py reports the video as "no identity"
        print(f"WARNING: {a.input}: no identified tracks; writing empty outputs")
        sio.Labels([], videos=labels.videos, skeletons=[sk]).save(
            a.output, restore_original_videos=False)
        if a.analysis_h5:
            import h5py
            with h5py.File(a.analysis_h5, "w") as f:
                f["tracks"] = np.zeros((0, 2, len(names), 0))
                f["track_names"] = np.array([], dtype="S")
                f["node_names"] = np.array([n.encode() for n in names])
        return
    f0, f1 = lfs[0].frame_idx, lfs[-1].frame_idx
    T = f1 - f0 + 1
    pts = {t.name: np.full((T, len(names), 2), np.nan) for t in tracks}
    score = {t.name: np.full(T, -np.inf) for t in tracks}
    for lf in lfs:
        for inst in lf.instances:
            if inst.track is None or inst.track.name not in pts:
                continue
            i = lf.frame_idx - f0
            s = float(getattr(inst, "score", 1.0) or 0.0)
            if s > score[inst.track.name][i]:  # one instance per identity per frame
                score[inst.track.name][i] = s
                pts[inst.track.name][i] = inst.numpy()

    report = []
    for name, p in pts.items():
        raw = int(np.isfinite(p[:, k_nodes[0]]).all(1).sum())
        sp = fi = 0
        for k in k_nodes:
            xy, s = despike(p[:, k], a.max_jump)
            xy, f = interpolate(xy, a.max_gap)
            p[:, k] = running_median(xy, a.window)
            sp, fi = sp + s, fi + f
        out = int(np.isfinite(p[:, k_nodes[0]]).all(1).sum())
        report.append(f"{name}: {raw} -> {out} frames ({nodes[0]}); spikes {sp}, filled {fi}")

    video = labels.videos[0]
    track_by = {t.name: t for t in tracks}
    new_lfs = []
    for i in range(T):
        insts = []
        for name, p in pts.items():
            if np.isfinite(p[i]).any():
                insts.append(sio.Instance.from_numpy(p[i], skeleton=sk, track=track_by[name]))
        if insts:
            new_lfs.append(sio.LabeledFrame(video=video, frame_idx=f0 + i, instances=insts))
    out = sio.Labels(new_lfs, videos=labels.videos, skeletons=[sk], tracks=tracks)
    out.save(a.output, restore_original_videos=False)
    if a.analysis_h5:
        sio.save_analysis_h5(out, a.analysis_h5) if hasattr(sio, "save_analysis_h5") else \
            sio.save_file(out, a.analysis_h5, format="analysis_h5")
    print("\n".join(report))
    print(f"wrote {a.output} ({len(new_lfs)} frames, nodes smoothed: {', '.join(nodes)})")


if __name__ == "__main__":
    main()
