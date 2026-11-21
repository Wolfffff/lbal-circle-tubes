"""Pick the most watchable clip per species and render it (scripts/render_video.py).

  python showcase.py --out results/showcase [--max-videos 15]
"""
import argparse
import random
import subprocess
import sys
from pathlib import Path

import cv2
import yaml

sys.path.insert(0, str(Path(__file__).parent))
from render_video import choose_window, load  # noqa: E402

SPECIES = {  # dataset: (title, identity stage)
    "baleicum": ("Lasioglossum baleicum", None),
    "aurata": ("Augochlorella aurata", "naps"),
    "pura": ("Augochlorella pura", "naps"),
    "zephyrus": ("Lasioglossum zephyrus", "naps"),
    "oenotherae": ("Lasioglossum oenotherae", "naps"),
    "melanderi": ("Nomia melanderi", "tracked"),
}

ap = argparse.ArgumentParser()
ap.add_argument("--out", default="results/showcase")
ap.add_argument("--model", default="v3")
ap.add_argument("--max-videos", type=int, default=15)
ap.add_argument("--seconds", type=float, default=30)
a = ap.parse_args()
cfg = yaml.safe_load(open("config/config.yaml"))
random.seed(0)
for ds, (title, stage) in SPECIES.items():
    if ds == "baleicum":
        cands = [(Path(a.out) / "baleicum/naps.slp",
                  "/scratch/gpfs/SKOCHER/mw0425/KL_SLEAP_LBAL/video_cropping/crop_output/20230623_101414_TR.mp4")]
    else:
        vdir = Path(cfg["datasets"][ds]["videos"][0])
        slps = sorted(Path(f"results/{a.model}/{ds}/{stage}").glob("*.slp"))
        slps = [s for s in slps if not s.name.endswith(".raw.slp") and not s.stem.startswith("20260622")]
        random.shuffle(slps)
        cands = [(s, vdir / f"{s.stem}.mp4") for s in slps[: a.max_videos]]
    best = None
    for slp, vid in cands:
        if not Path(slp).exists():
            continue
        frames, names, _ = load(str(slp))
        n = min(int(cv2.VideoCapture(str(vid)).get(7)), max(frames) + 1)
        start, score = choose_window(frames, names, n, int(a.seconds * 20), stage != "tracked")
        if best is None or score > best[0]:
            best = (score, slp, vid, start)
    if best is None:
        print(ds, "no candidates"); continue
    score, slp, vid, start = best
    stem = Path(vid).stem
    sub = f"{stem}   ·   sleap-nn pose + NAPS colour-tag identity" if stage != "tracked" else f"{stem}   ·   sleap-nn pose + tracking"
    o = Path(a.out)
    print(f"{ds}: {stem} from frame {start} (score {score:.2f})", flush=True)
    subprocess.run([sys.executable, str(Path(__file__).parent / "render_video.py"), "--slp", str(slp),
                    "--video", str(vid), "--out", str(o / f"{ds}.mp4"), "--title", title,
                    "--subtitle", sub, "--start", str(start), "--seconds", str(a.seconds),
                    "--gif", str(o / f"{ds}.gif"), "--poster", str(o / f"{ds}.png")], check=True)
