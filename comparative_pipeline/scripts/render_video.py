"""Render a short, presentation-quality tracking video (MP4, H.264) plus optional GIF and poster.

- each bee in its identity colour (purple dot = magenta, black dot = cyan; SLEAP tracks get neutral
  colours when there is no identity), with a soft glow, skeleton, fading trail and a label with its
  live speed
- poses are cleaned and smoothed with the `movement` package (confidence filter, gap interpolation,
  rolling median, Savitzky-Golay); speed from movement.kinematics.compute_speed
- trails get thicker/brighter with speed; a strip at the bottom plots both bees' speed over the clip
- head-to-head contacts are ringed; --focus <id> dims everything outside a spotlight on one bee
- frames are contrast-normalised (CLAHE) so dark and bright sessions look alike
- the clip window is chosen automatically (both bees present, close approaches, movement) unless
  --start is given

  python render_video.py --slp naps.slp --video v.mp4 --out clip.mp4 --title "Lasioglossum zephyrus"
      [--subtitle ...] [--seconds 30] [--start F] [--focus purple] [--gif x.gif] [--poster x.png]
"""

import argparse

import cv2
import imageio_ffmpeg
import numpy as np
import sleap_io as sio
from matplotlib import font_manager
from PIL import Image, ImageDraw, ImageFont
from movement.filtering import (
    filter_by_confidence,
    interpolate_over_time,
    rolling_filter,
    savgol_filter,
)
from movement.io import load_poses
from movement.kinematics import compute_speed

ID_COLOURS = {"purple": (232, 74, 237), "black": (34, 211, 238)}  # RGB
TRACK_COLOURS = [(250, 204, 21), (74, 222, 128), (248, 113, 113), (147, 197, 253)]
FPS = 20


def font(size, style="normal", weight="normal"):
    fp = font_manager.FontProperties(family="DejaVu Sans", style=style, weight=weight)
    return ImageFont.truetype(font_manager.findfont(fp), size)


def ident(tname):
    """(key, label, colour) for a track name, or None."""
    if tname is None:
        return None
    if tname.startswith("ArUcoTag#"):
        k = tname.split("#", 1)[1]
        k = {"0": "black", "1": "purple"}.get(k, k)  # raw NAPS ids (naps-color-decodes defaults)
        return (k, f"{k} dot", ID_COLOURS[k]) if k in ID_COLOURS else None
    tail = tname.split("_")[-1]
    i = int(tail) if tail.isdigit() else 0
    return tname, f"bee {i + 1}", TRACK_COLOURS[i % len(TRACK_COLOURS)]


def load(slp_path):
    labels = sio.load_slp(slp_path, open_videos=False)
    sk = labels.skeleton
    names = sk.node_names
    edges = [(names.index(a.name), names.index(b.name)) for a, b in sk.edges]
    frames = {lf.frame_idx: [(i.track.name if i.track else None, i.numpy()) for i in lf.instances]
              for lf in labels.labeled_frames}  # fmt: skip
    return frames, names, edges


def choose_window(frames, names, n_frames, length, identity):
    th, hd = names.index("thorax"), names.index("head")
    both, close, speed, last = (np.zeros(n_frames), np.zeros(n_frames), np.zeros(n_frames), {})
    for f in range(n_frames):
        rows = [(ident(t), p) for t, p in frames.get(f, []) if ident(t)]
        keys = {r[0][0] for r in rows}
        both[f] = len(rows) == 2 and len(keys) == 2 and (not identity or keys <= set(ID_COLOURS))
        if len(rows) == 2:
            a, b = rows[0][1], rows[1][1]
            d = np.nanmin([np.hypot(*(a[hd] - b[hd])), np.hypot(*(a[th] - b[th]))])
            close[f] = np.isfinite(d) and d < 110
        for (k, _, _), p in rows:
            if k in last and np.all(np.isfinite(p[th])) and np.all(np.isfinite(last[k])):
                speed[f] += np.hypot(*(p[th] - last[k]))
            last[k] = p[th]
    best, best_s = 0, -1.0
    for s in range(0, max(1, n_frames - length), 25):
        w = slice(s, s + length)
        bf = both[w].mean()
        if bf < 0.85:
            continue
        sc = bf * (1 + 2 * min(close[w].mean() * 4, 1) + min(np.median(speed[w]) / 4, 1))
        if sc > best_s:
            best, best_s = s, sc
    return best, best_s


def trajectories(slp_path, start, length, args):
    """Smoothed poses and speed with the `movement` package.

    Returns {key: (label, colour, (T, nodes, 2) positions in px, (T,) thorax speed in px/s)}.
    Steps: confidence filter -> interpolate gaps <= --max-gap frames -> rolling median ->
    Savitzky-Golay; speed = movement.kinematics.compute_speed of the smoothed thorax.
    """
    ds = load_poses.from_sleap_file(slp_path, fps=FPS).isel(time=slice(start, start + length))
    pos = filter_by_confidence(ds.position, ds.confidence, threshold=args.min_confidence)
    pos = interpolate_over_time(pos, max_gap=args.max_gap)
    if args.median_window > 1:
        pos = rolling_filter(pos, args.median_window, statistic="median", min_periods=2)
    if args.savgol_window > 2:
        pos = savgol_filter(pos, args.savgol_window, polyorder=2, mode="nearest")
    speed = compute_speed(pos.sel(keypoint="thorax"))  # px per second (time is in s)
    out = {}
    for ind in pos.individual.values:
        idt = ident(str(ind))
        if idt is None:
            continue
        k, lab, col = idt
        X = pos.sel(individual=ind).transpose("time", "keypoint", "space").values.astype(float)
        out[k] = (lab, col, X, speed.sel(individual=ind).values.astype(float))
    return out


def crop_box(trajs, th, w, h, pad=120, min_size=560):
    pts = np.concatenate([X[:, th] for _, _, X, _ in trajs.values()]) if trajs else np.zeros((0, 2))
    pts = pts[np.isfinite(pts).all(1)]
    if not len(pts):
        return 0, 0, min(w, h)
    lo, hi = np.percentile(pts, 1, 0) - pad, np.percentile(pts, 99, 0) + pad
    size = int(min(max((hi - lo).max(), min_size), w, h))
    cx, cy = (lo + hi) / 2
    return int(np.clip(cx - size / 2, 0, w - size)), int(np.clip(cy - size / 2, 0, h - size)), size


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--slp", required=True)
    ap.add_argument("--video", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--title", default="")
    ap.add_argument("--subtitle", default="")
    ap.add_argument("--seconds", type=float, default=30)
    ap.add_argument("--start", type=int)
    ap.add_argument("--size", type=int, default=960)
    ap.add_argument("--trail", type=int, default=60, help="trail length (frames)")
    ap.add_argument("--min-confidence", type=float, default=0.2, help="drop points below this score")
    ap.add_argument("--max-gap", type=int, default=8, help="interpolate gaps up to this (frames)")
    ap.add_argument("--median-window", type=int, default=5, help="rolling median (frames)")
    ap.add_argument("--savgol-window", type=int, default=7, help="Savitzky-Golay window (frames)")
    ap.add_argument("--mm-per-px", type=float, default=0.0946)
    ap.add_argument("--focus", help="spotlight one bee (e.g. purple, black, track_0)")
    ap.add_argument("--gif", help="also write a GIF (first 8 s, half size, 10 fps)")
    ap.add_argument("--poster", help="also write a PNG of the middle frame")
    a = ap.parse_args()

    frames, names, edges = load(a.slp)
    cap = cv2.VideoCapture(a.video)
    W, H = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH)), int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    n_frames = min(int(cap.get(cv2.CAP_PROP_FRAME_COUNT)), max(frames) + 1)
    length = int(a.seconds * FPS)
    identity = any(t and t.startswith("ArUcoTag#") for r in frames.values() for t, _ in r)
    start = a.start
    if start is None:
        start, score = choose_window(frames, names, n_frames, length, identity)
        print(f"window {start}-{start + length} (score {score:.2f})")
    length = min(length, n_frames - start)
    th, hd, tg, ab = (names.index(n) for n in ("thorax", "head", "tag", "abdomen"))
    trajs = trajectories(a.slp, start, length, a)
    keys = sorted(trajs)
    speed = {k: np.nan_to_num(trajs[k][3]) * a.mm_per_px for k in keys}  # mm/s
    vmax = max([np.nanpercentile(v, 98) for v in speed.values()] + [1.0])

    x0, y0, size = crop_box(trajs, th, W, H)
    S = a.size / size
    strip_h = 92
    f_title, f_sub, f_lab, f_small = (font(30, "italic", "bold"), font(18), font(16, weight="bold"),
                                      font(15))  # fmt: skip
    clahe = cv2.createCLAHE(clipLimit=2.0, tileGridSize=(8, 8))
    out_h = a.size + strip_h
    writer = imageio_ffmpeg.write_frames(a.out, (a.size, out_h), fps=FPS, codec="libx264",
                                         pix_fmt_out="yuv420p", output_params=["-crf", "18"],
                                         macro_block_size=4)  # fmt: skip
    writer.send(None)
    gif_frames = []
    cap.set(cv2.CAP_PROP_POS_FRAMES, start)

    def P(p):
        return (p[0] - x0) * S, (p[1] - y0) * S

    def Pi(p):
        x, y = P(p)
        return int(round(x)), int(round(y))

    for t in range(length):
        ok, img = cap.read()
        if not ok:
            break
        f = start + t
        g = clahe.apply(img[y0 : y0 + size, x0 : x0 + size, 0])
        g = cv2.resize(g, (a.size, a.size), interpolation=cv2.INTER_CUBIC)
        canvas = cv2.cvtColor(g, cv2.COLOR_GRAY2RGB).astype(np.float32) * 0.9
        present = {k: trajs[k][2][t] for k in keys if np.isfinite(trajs[k][2][t][th]).all()}

        # spotlight on one bee
        if a.focus and a.focus in present:
            mask = np.zeros((a.size, a.size), np.float32)
            cv2.circle(mask, Pi(present[a.focus][th]), int(110 * S), 1.0, -1, cv2.LINE_AA)
            mask = cv2.GaussianBlur(mask, (0, 0), 40 * S)[..., None]
            canvas = canvas * (0.35 + 0.65 * np.clip(mask * 1.6, 0, 1))
        # glow under each bee
        glow = np.zeros_like(canvas)
        for k, p in present.items():
            col = trajs[k][1]
            body = [p[n] for n in (hd, th, ab) if np.isfinite(p[n]).all()]
            for q0, q1 in zip(body[:-1], body[1:]):
                cv2.line(glow, Pi(q0), Pi(q1), col, int(34 * S) + 6, cv2.LINE_AA)
        glow = cv2.GaussianBlur(glow, (0, 0), 14 * S + 4)
        canvas = np.clip(canvas + 0.55 * glow, 0, 255).astype(np.uint8)

        # trails (thickness/brightness with speed)
        for k in keys:
            col = trajs[k][1]
            X = trajs[k][2][max(0, t - a.trail) : t + 1, th]
            v = speed[k][max(0, t - a.trail) : t + 1]
            n = len(X)
            for i in range(1, n):
                if not (np.isfinite(X[i]).all() and np.isfinite(X[i - 1]).all()):
                    continue
                age = i / n
                sp = min(v[i] / vmax, 1.0)
                alpha = age * (0.45 + 0.55 * sp)
                c = tuple(int(alpha * col[j] + (1 - alpha) * 70) for j in range(3))
                cv2.line(canvas, Pi(X[i - 1]), Pi(X[i]), c, max(1, int(round(age * (2 + 4 * sp)))),
                         cv2.LINE_AA)  # fmt: skip
        # contact ring (heads close)
        if len(present) == 2:
            pa, pb = present.values()
            if np.isfinite([pa[hd], pb[hd]]).all() and np.hypot(*(pa[hd] - pb[hd])) < 45:
                cv2.circle(canvas, Pi((pa[hd] + pb[hd]) / 2), int(40 * S), (255, 255, 255), 2,
                           cv2.LINE_AA)  # fmt: skip
        # skeletons
        for k, p in present.items():
            col = trajs[k][1]
            dark = tuple(int(c * 0.35) for c in col)
            for i, j in edges:
                if np.isfinite(p[[i, j]]).all():
                    cv2.line(canvas, Pi(p[i]), Pi(p[j]), dark, 5, cv2.LINE_AA)
                    cv2.line(canvas, Pi(p[i]), Pi(p[j]), col, 2, cv2.LINE_AA)
            for n_ in range(len(names)):
                if np.isfinite(p[n_]).all():
                    cv2.circle(canvas, Pi(p[n_]), 3, (255, 255, 255), -1, cv2.LINE_AA)
            if np.isfinite(p[tg]).all():
                cv2.circle(canvas, Pi(p[tg]), int(7 * S) + 3, (255, 255, 255), 2, cv2.LINE_AA)

        im = Image.new("RGB", (a.size, out_h), (12, 12, 16))
        im.paste(Image.fromarray(canvas), (0, 0))
        d = ImageDraw.Draw(im, "RGBA")
        # labels with live speed
        for k, p in present.items():
            lab, col = trajs[k][0], trajs[k][1]
            ok_ = np.isfinite(p).all(1)
            cx = P(p[ok_].mean(0))[0]
            top = min(P(q)[1] for q in p[ok_])
            txt = f"{lab}  {speed[k][t]:.1f} mm/s"
            tw = d.textlength(txt, font=f_lab)
            bx, by = cx - tw / 2 - 10, max(80, top - 40)
            d.rounded_rectangle([bx, by, bx + tw + 20, by + 26], radius=13, fill=(12, 12, 16, 200),
                                outline=col + (255,), width=2)  # fmt: skip
            d.text((bx + 10, by + 4), txt, font=f_lab, fill=col + (255,))
        # header
        d.rectangle([0, 0, a.size, 74], fill=(10, 10, 14, 180))
        d.text((18, 8), a.title, font=f_title, fill=(255, 255, 255, 255))
        d.text((18, 46), a.subtitle, font=f_sub, fill=(200, 200, 210, 255))
        sec = f / FPS
        tstr = f"{int(sec // 60):02d}:{sec % 60:04.1f}"
        d.text((a.size - 18 - d.textlength(tstr, font=f_sub), 46), tstr, font=f_sub,
               fill=(200, 200, 210, 255))  # fmt: skip
        # speed strip
        y_top, y_bot, x_l, x_r = a.size + 22, out_h - 22, 96, a.size - 18
        d.text((14, a.size + 8), "speed", font=f_small, fill=(170, 170, 180, 255))
        d.text((14, a.size + 26), f"{vmax:.0f} mm/s", font=f_small, fill=(120, 120, 130, 255))
        d.line([x_l, y_bot, x_r, y_bot], fill=(70, 70, 80, 255), width=1)
        for k in keys:
            col = trajs[k][1]
            v = speed[k]
            xs = x_l + (x_r - x_l) * np.arange(length) / max(length - 1, 1)
            ys = y_bot - (y_bot - y_top) * np.clip(v / vmax, 0, 1)
            pts = list(zip(xs[: t + 1], ys[: t + 1]))
            if len(pts) > 1:
                d.line(pts, fill=col + (255,), width=2)
            if len(xs[t + 1 :]) > 1:
                d.line(list(zip(xs[t + 1 :], ys[t + 1 :])), fill=col + (60,), width=1)
        d.text((x_l, out_h - 19), f"thorax speed, mm/s at {a.mm_per_px} mm/px", font=font(12),
               fill=(110, 110, 120, 255))  # fmt: skip
        cx_ = x_l + (x_r - x_l) * t / max(length - 1, 1)
        d.line([cx_, y_top - 4, cx_, y_bot], fill=(255, 255, 255, 160), width=1)
        if not identity:
            d.text((a.size - 18 - d.textlength("no identity: tags indistinguishable", font=f_small),
                    a.size + 4), "no identity: tags indistinguishable", font=f_small,
                   fill=(200, 200, 210, 255))  # fmt: skip

        frame = np.asarray(im)
        writer.send(np.ascontiguousarray(frame))
        if a.poster and t == length // 2:
            im.save(a.poster)
        if a.gif and t < 8 * FPS and t % 2 == 0:
            gif_frames.append(im.resize((a.size // 2, out_h // 2), Image.LANCZOS))
    writer.close()
    if a.gif and gif_frames:
        pal = [g.quantize(colors=160, method=Image.Quantize.MEDIANCUT) for g in gif_frames]
        pal[0].save(a.gif, save_all=True, append_images=pal[1:], duration=100, loop=0, optimize=True)
    print(f"wrote {a.out} ({length} frames, crop {size}px at ({x0},{y0}))")


if __name__ == "__main__":
    main()
