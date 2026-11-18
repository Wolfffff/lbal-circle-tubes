"""Rename NAPS tag tracks, e.g. ArUcoTag#0 -> ArUcoTag#black (the names the filter/analysis use),
and drop the sleap-io identity data so SLEAP 1.x can read the file.

  python rename_tracks.py in.slp out.slp 0=black 1=purple
"""

import sys

import sleap_io as sio


def main():
    src, dst, *pairs = sys.argv[1:]
    names = dict(p.split("=", 1) for p in pairs)
    labels = sio.load_slp(src, open_videos=False)
    for t in labels.tracks:
        if t.name.startswith("ArUcoTag#"):
            tag = t.name.split("#", 1)[1]
            if tag in names:
                t.name = f"ArUcoTag#{names[tag]}"
    # NAPS (joint engine) also stores sleap-io identities, which bump the file to SLP format 2.5;
    # SLEAP 1.x (filter_nodes.py) cannot read that. The tracks carry the same assignment.
    for lf in labels.labeled_frames:
        for inst in lf.instances:
            inst.identity = None
            inst.identity_score = None
    labels.identities.clear()
    labels.save(dst, restore_original_videos=False)
    print(src, "->", dst, [t.name for t in labels.tracks])


if __name__ == "__main__":
    main()
