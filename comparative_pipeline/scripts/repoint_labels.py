"""Re-point a .slp's video paths (e.g. 'G:/Shared drives/...') to local copies.

Each video is looked up by basename in the given directories (first match wins).
Fails if a video that has labeled frames can't be found/read.

  python repoint_labels.py in.slp out.slp DIR [DIR ...]
"""

import os
import sys
from pathlib import Path

import sleap_io as sio


def main(src, dst, *dirs):
    labels = sio.load_slp(src, open_videos=False)
    used = {id(lf.video) for lf in labels.labeled_frames if lf.user_instances}
    missing = []
    for v in labels.videos:
        name = Path(v.filename.replace("\\", "/")).name
        hit = next((os.path.join(d, name) for d in dirs
                    if os.access(os.path.join(d, name), os.R_OK)), None)
        if hit:
            v.replace_filename(os.path.abspath(hit), open=False)
        elif id(v) in used:
            missing.append(name)
    if missing:
        sys.exit(f"{src}: {len(missing)} labeled videos not found/readable, e.g. {missing[:3]}")
    Path(dst).parent.mkdir(parents=True, exist_ok=True)
    sio.save_slp(labels, dst)
    n = sum(len(lf.user_instances) for lf in labels.labeled_frames)
    print(f"{src} -> {dst}: {len(labels.labeled_frames)} frames, {n} user instances")


if __name__ == "__main__":
    main(*sys.argv[1:])
