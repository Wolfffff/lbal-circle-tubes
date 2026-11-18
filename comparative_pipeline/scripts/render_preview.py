"""Render predictions on video frames: skeleton + identity (track) colour + tag disc.

  python render_preview.py --slp naps.slp --video v.mp4 --out-png sheet.png [--out-gif clip.gif]
      [--frames 0 500 1000 ...] [--n 12] [--gif-start 0 --gif-len 100 --gif-step 2] [--crop 360]

The contact sheet shows --n evenly spaced frames (or --frames), each cropped around the bees.
The GIF is a short clip (every --gif-step frame) downscaled to --gif-width px.
"""

import argparse

import cv2
import numpy as np
import sleap_io as sio
from PIL import Image

PALETTE = {  # BGR
    "black": (255, 200, 0),     # bee with the black dot  -> cyan-blue
    "purple": (200, 40, 230),   # bee with the bright dot -> magenta
}
OTHER = [(0, 200, 255), (0, 255, 120), (80, 80, 255), (255, 255, 255)]
NOID = (150, 150, 150)


def colour_of(name, cache):
    if name is None:
        return NOID, "?"
    key = name.split("#", 1)[1] if "#" in name else name
    if key in PALETTE:
        return PALETTE[key], key
    if key not in cache:
        cache[key] = OTHER[len(cache) % len(OTHER)]
    return cache[key], key


def draw(img, lf, skeleton, cache, tag_k, scale=1.0):
    edges = [(skeleton.node_names.index(a.name), skeleton.node_names.index(b.name))
             for a, b in skeleton.edges]  # fmt: skip
    t = max(1, int(round(2 * scale)))
    for inst in lf.instances:
        pts = inst.numpy()
        col, label = colour_of(inst.track.name if inst.track else None, cache)
        for i, j in edges:
            if np.all(np.isfinite(pts[[i, j]])):
                cv2.line(img, tuple(np.int32(pts[i])), tuple(np.int32(pts[j])), col, t, cv2.LINE_AA)
        for p in pts:
            if np.all(np.isfinite(p)):
                cv2.circle(img, tuple(np.int32(p)), t + 1, col, -1, cv2.LINE_AA)
        if tag_k is not None and np.all(np.isfinite(pts[tag_k])):
            cv2.circle(img, tuple(np.int32(pts[tag_k])), int(6 * scale) + 4, (255, 255, 255), t,
                       cv2.LINE_AA)  # fmt: skip
        ok = np.isfinite(pts).all(1)
        if ok.any():
            x, y = np.int32(pts[ok].min(0)) - [0, 8]
            cv2.putText(img, label, (int(x), int(y)), cv2.FONT_HERSHEY_SIMPLEX, 0.7 * scale, col,
                        t, cv2.LINE_AA)  # fmt: skip


def crop_box(lf, w, h, size):
    pts = np.concatenate([i.numpy() for i in lf.instances]) if lf.instances else np.zeros((0, 2))
    pts = pts[np.isfinite(pts).all(1)]
    cx, cy = (pts.mean(0) if len(pts) else (w / 2, h / 2))
    span = max(size, int(np.ptp(pts, 0).max() + 120) if len(pts) else size)
    span = min(span, w, h)
    x0 = int(np.clip(cx - span / 2, 0, w - span))
    y0 = int(np.clip(cy - span / 2, 0, h - span))
    return x0, y0, span


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--slp", required=True)
    ap.add_argument("--video", required=True)
    ap.add_argument("--out-png")
    ap.add_argument("--out-gif")
    ap.add_argument("--frames", type=int, nargs="*")
    ap.add_argument("--n", type=int, default=12)
    ap.add_argument("--cols", type=int, default=4)
    ap.add_argument("--crop", type=int, default=420)
    ap.add_argument("--tile", type=int, default=320)
    ap.add_argument("--title", default="")
    ap.add_argument("--gif-start", type=int)
    ap.add_argument("--gif-len", type=int, default=200)
    ap.add_argument("--gif-step", type=int, default=2)
    ap.add_argument("--gif-width", type=int, default=480)
    a = ap.parse_args()

    labels = sio.load_slp(a.slp, open_videos=False)
    sk = labels.skeleton
    tag_k = sk.node_names.index("tag") if "tag" in sk.node_names else None
    by_frame = {lf.frame_idx: lf for lf in labels.labeled_frames}
    cap = cv2.VideoCapture(a.video)
    W, H = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH)), int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    cache = {}

    def frame(i):
        cap.set(cv2.CAP_PROP_POS_FRAMES, i)
        ok, img = cap.read()
        return img if ok else None

    if a.out_png:
        frames = a.frames or list(np.linspace(min(by_frame), max(by_frame), a.n).astype(int))
        tiles = []
        for fi in frames:
            img = frame(fi)
            lf = by_frame.get(fi)
            if img is None:
                continue
            if lf is not None:
                draw(img, lf, sk, cache, tag_k)
                x0, y0, s = crop_box(lf, W, H, a.crop)
                img = img[y0 : y0 + s, x0 : x0 + s]
            tile = cv2.resize(img, (a.tile, a.tile), interpolation=cv2.INTER_AREA)
            cv2.putText(tile, str(fi), (6, 20), cv2.FONT_HERSHEY_SIMPLEX, 0.55, (255, 255, 255), 2)
            cv2.putText(tile, str(fi), (6, 20), cv2.FONT_HERSHEY_SIMPLEX, 0.55, (0, 0, 0), 1)
            tiles.append(tile)
        while len(tiles) % a.cols:
            tiles.append(np.zeros_like(tiles[0]))
        rows = [np.hstack(tiles[i : i + a.cols]) for i in range(0, len(tiles), a.cols)]
        sheet = np.vstack(rows)
        if a.title:
            bar = np.full((34, sheet.shape[1], 3), 255, np.uint8)
            cv2.putText(bar, a.title, (8, 23), cv2.FONT_HERSHEY_SIMPLEX, 0.65, (0, 0, 0), 1,
                        cv2.LINE_AA)  # fmt: skip
            sheet = np.vstack([bar, sheet])
        cv2.imwrite(a.out_png, sheet)
        print("wrote", a.out_png, sheet.shape)

    if a.out_gif:
        start = a.gif_start if a.gif_start is not None else min(by_frame)
        cap.set(cv2.CAP_PROP_POS_FRAMES, start)
        imgs = []
        for fi in range(start, start + a.gif_len):
            ok, img = cap.read()
            if not ok:
                break
            if (fi - start) % a.gif_step:
                continue
            lf = by_frame.get(fi)
            scale = W / a.gif_width
            if lf is not None:
                draw(img, lf, sk, cache, tag_k, scale=scale * 0.6)
            img = cv2.resize(img, (a.gif_width, int(H * a.gif_width / W)),
                             interpolation=cv2.INTER_AREA)  # fmt: skip
            imgs.append(Image.fromarray(img[:, :, ::-1]))
        if imgs:
            imgs[0].save(a.out_gif, save_all=True, append_images=imgs[1:], loop=0,
                         duration=int(1000 / 20 * a.gif_step), optimize=True)  # fmt: skip
            print("wrote", a.out_gif, len(imgs), "frames")


if __name__ == "__main__":
    main()
