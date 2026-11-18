import argparse
import copy

import sleap
import sleap.instance
import numpy as np
from scipy.optimize import linear_sum_assignment

# ---------------------------------------------------------------------
# DEBUG UTILITIES
# ---------------------------------------------------------------------

DEBUG = False       # toggled by --debug
DEBUG_FRAME = None  # focus debug around this frame (optional)
DEBUG_RANGE = 0     # +/- range around DEBUG_FRAME
CURRENT_FRAME = None


def dbg_allowed():
    """Return True if debug output should be printed at CURRENT_FRAME."""
    if not DEBUG:
        return False
    if DEBUG_FRAME is None:
        return True
    if CURRENT_FRAME is None:
        return False
    return abs(CURRENT_FRAME - DEBUG_FRAME) <= DEBUG_RANGE


def dbg(*msg):
    """Generic debug print."""
    if dbg_allowed():
        print("[DBG]", *msg)


def dbg_frame_header(f):
    """Print frame header if debugging this frame."""
    if not DEBUG:
        return
    if DEBUG_FRAME is None or abs(f - DEBUG_FRAME) <= DEBUG_RANGE:
        print(f"\n================ FRAME {f} ================")


def dbg_assignment(tag, prev_theta, new_theta, dtheta, max_dtheta, accepted, note=""):
    if not dbg_allowed():
        return
    status = "ACCEPT" if accepted else "REJECT"
    note_str = f" ({note})" if note else ""
    print(
        f"[ASSIGN]{note_str} Frame {CURRENT_FRAME}, {tag}: "
        f"prev θ={prev_theta:.3f}, new θ={new_theta:.3f}, "
        f"Δθ={dtheta:.3f} vs max {max_dtheta:.3f} → {status}"
    )


def debug_dump_selected_frames(labels, node_name, tag_names, skip_frames):
    """
    Dump instance data for ONLY frames within DEBUG_FRAME ± DEBUG_RANGE.
    Uses the global DEBUG, DEBUG_FRAME, DEBUG_RANGE flags.
    """
    global DEBUG, DEBUG_FRAME, DEBUG_RANGE

    if not DEBUG or DEBUG_FRAME is None:
        print("\n[DEBUG-DUMP] No DEBUG_FRAME specified — skipping dump.\n")
        return

    print("\n================ DEBUG DUMP OF FINAL TRACKS ================")

    # get frames
    try:
        lf_all = labels.labeled_frames
    except TypeError:
        lf_all = labels.labeled_frames()

    start = max(0, DEBUG_FRAME - DEBUG_RANGE)
    end = min(len(lf_all) - 1, DEBUG_FRAME + DEBUG_RANGE)

    for f in range(start, end + 1):
        lf = lf_all[f]

        prefix = ""
        if f < skip_frames:
            prefix = "[PRE-SKIP]"
        elif f == skip_frames:
            prefix = ">> [FIRST PROCESSED FRAME] <<"

        print(f"\nFrame {f} {prefix}")
        print("  #instances:", len(lf.instances))

        # loop through instances
        for inst in lf.instances:
            track = inst.track.name if inst.track else "(no track)"
            node = inst[node_name]

            if node is None or node.x is None or node.y is None:
                print(f"    Track={track}  node=MISSING")
            else:
                print(
                    f"    Track={track}"
                    f"  x={float(node.x):.3f}"
                    f"  y={float(node.y):.3f}"
                    f"  visible={node.visible}"
                )


# ---------------------------------------------------------------------
# TRACK NAME DETECTION
# ---------------------------------------------------------------------

def get_valid_track_names(labels):
    """
    Return ONLY the two *base* tracks (e.g. ArUcoTag#black and ArUcoTag#purple).

    Ignore any tracks created by segment splitting such as:
        ArUcoTag#black_seg1
        ArUcoTag#purple_seg2

    Behavior:
      - Collect all track names.
      - Filter out names containing 'none'.
      - Filter out segment tracks ('_seg...').
      - Expect exactly 2 base names.
    """
    names = [t.name for t in labels.tracks]
    dbg("All track names:", names)

    # Remove "none", empty, whitespace
    names = [n for n in names if n and "none" not in n.lower() and n.strip()]

    # Remove tracks created by segment splitting
    base_names = [n for n in names if "_seg" not in n]

    if len(base_names) != 2:
        raise ValueError(
            f"Expected 2 base tracks, found {len(base_names)}: {base_names}\n"
            f"(Full set was: {names})"
        )

    dbg("[TRACKS] Using base track names:", base_names)
    return base_names


# ---------------------------------------------------------------------
# VISIBILITY FILTER
# ---------------------------------------------------------------------

def hide_all_but_node(labels, node_name):
    """
    Set only `node_name` visible in every instance; hide all other nodes.
    """
    skel = labels.skeleton
    node_idx = None
    for i, n in enumerate(skel.nodes):
        if n.name == node_name:
            node_idx = i
            break
    if node_idx is None:
        raise ValueError(f"Node {node_name} not found in skeleton.")

    dbg("Skeleton nodes:", [n.name for n in skel.nodes])
    dbg(f"Keeping only node index {node_idx} ({node_name}) visible")

    try:
        lf_list = labels.labeled_frames
    except TypeError:
        lf_list = labels.labeled_frames()

    for lf in lf_list:
        for inst in lf.instances:
            for i, pt in enumerate(inst.points):
                pt.visible = (i == node_idx)

    return labels


# ---------------------------------------------------------------------
# GEOMETRY HELPERS
# ---------------------------------------------------------------------

def angle_of_point(pt, center):
    if pt is None or np.isnan(pt).any():
        return np.nan
    v = pt - center
    return np.arctan2(v[1], v[0])


def angular_distance(a, b):
    """Smallest absolute angular distance between two angles."""
    d = a - b
    d = (d + np.pi) % (2 * np.pi) - np.pi
    return abs(d)


# ---------------------------------------------------------------------
# INTERPOLATION AND SMOOTHING
# ---------------------------------------------------------------------

def interpolate_nan(traj, max_gap=None):
    """
    Fill NaNs in (T,2) trajectory:

    - Leading NaNs  -> filled with first valid value
    - Trailing NaNs -> filled with last valid value
    - Interior NaNs -> linearly interpolated *only if* the gap length
                       is <= max_gap (if max_gap is not None).
                       Longer gaps are left as NaN to avoid long chords.
    """
    out = traj.copy().astype(float)
    T = len(out)
    x_idx = np.arange(T)

    for dim in range(2):
        y = out[:, dim]
        valid = ~np.isnan(y)
        if not valid.any():
            continue

        valid_idx = np.where(valid)[0]
        first = valid_idx[0]
        last = valid_idx[-1]

        # Leading
        y[:first] = y[first]
        # Trailing
        y[last + 1:] = y[last]

        # Interior gaps
        interior = np.isnan(y) & (x_idx > first) & (x_idx < last)

        # Work gap by gap
        i = first
        while i < last:
            if not interior[i]:
                i += 1
                continue

            # start of a NaN run
            start = i
            while i <= last and interior[i]:
                i += 1
            end = i - 1  # last NaN index in this run

            gap_len = end - start + 1

            if (max_gap is None) or (gap_len <= max_gap):
                # interpolate between y[start-1] and y[end+1]
                left = start - 1
                right = end + 1
                y[start:end + 1] = np.interp(
                    x_idx[start:end + 1],
                    [left, right],
                    [y[left], y[right]],
                )
            else:
                # leave as NaN → no big straight segment
                continue

        out[:, dim] = y

    return out


def smooth_traj(traj, window):
    """Simple moving-average smoothing over each dimension."""
    if window <= 1:
        return traj

    out = traj.copy()
    kernel = np.ones(window, dtype=float) / float(window)

    for dim in range(2):
        out[:, dim] = np.convolve(traj[:, dim], kernel, mode="same")

    return out


def clip_jumps_theta(traj, center, max_dtheta):
    """
    Theta-based post-hoc clipping:
    - Convert each point to angle
    - If Δθ > max_dtheta, replace with previous position.
    """
    out = traj.copy()
    T = len(out)

    thetas = np.array([
        angle_of_point(pt, center) if not np.isnan(pt).any() else np.nan
        for pt in out
    ])

    for t in range(1, T):
        if np.isnan(thetas[t]) or np.isnan(thetas[t - 1]):
            continue

        dtheta = angular_distance(thetas[t], thetas[t - 1])
        if dtheta > max_dtheta:
            if dbg_allowed():
                print(
                    f"[THETA-CLIP] Frame {t}: Δθ={dtheta:.3f} > "
                    f"limit {max_dtheta:.3f} → holding previous position"
                )
            out[t] = out[t - 1]
            thetas[t] = thetas[t - 1]

    return out


# 🔥 NEW: explicit XY jump clipping regardless of angles
def clip_jumps_xy(traj, max_jump):
    """
    Post-hoc XY clipping with LOST-TRACK RECOVERY.
    If a jump > max_jump occurs, mark that frame as NaN.
    Subsequent frames are allowed to reacquire instead of freezing.
    """
    out = traj.copy().astype(float)
    T = len(out)

    for t in range(1, T):

        # If previous frame was invalid, allow reacquisition
        if np.isnan(out[t - 1]).any():
            continue

        if np.isnan(out[t]).any():
            continue

        d = np.linalg.norm(out[t] - out[t - 1])

        if d > max_jump:
            if dbg_allowed():
                print(f"[XY-CLIP] Frame {t}: dXY={d:.1f} too large → marking NaN (lost track)")

            # LOST TRACK RECOVERY: mark this as invalid so next frame may reacquire
            out[t] = np.array([np.nan, np.nan])

    return out



def ensure_instance_has_all_nodes(inst):
    """
    Ensure instance has exactly one Point per skeleton node.
    Safe for all SLEAP versions where instance.points is immutable.
    Returns a corrected Instance object.
    """
    skel = inst.skeleton
    required = len(skel.nodes)

    # Convert tuple -> list
    pts_old = list(inst.points)

    # Start new list with NaN placeholders
    pts_new = [
        sleap.instance.Point(np.nan, np.nan, visible=False)
        for _ in range(required)
    ]

    # Copy existing coords into matching slots
    for i in range(min(len(pts_old), required)):
        old_pt = pts_old[i]
        pts_new[i] = sleap.instance.Point(
            x=old_pt.x if old_pt.x is not None else np.nan,
            y=old_pt.y if old_pt.y is not None else np.nan,
            visible=old_pt.visible if hasattr(old_pt, "visible") else True,
        )

    # Build a brand new instance — safe for all SLEAP versions
    new_inst = sleap.instance.Instance(
        points=pts_new,
        skeleton=skel,
        track=inst.track,
    )

    # Try to restore optional attributes
    if hasattr(inst, "score"):
        try:
            new_inst.score = inst.score
        except Exception:
            pass

    if hasattr(inst, "temporal_window"):
        new_inst.temporal_window = inst.temporal_window

    return new_inst


def clear_track_cache(labels):
    for t in labels.tracks:
        # SLEAP caches per-frame point arrays here
        if hasattr(t, "points"):
            t.points = None
        if hasattr(t, "instance_ids"):
            t.instance_ids = None
        if hasattr(t, "tracklet"):
            t.tracklet = None
        if hasattr(t, "tracking_scores"):
            t.tracking_scores = None


def full_reset_track_object(track):
    """Aggressively clear all cached tracking data."""
    # SLEAP tracks sometimes store frame indices or cached properties
    for attr in ["_instance_idxs", "_last_instance", "_last_frame", "last_lf"]:
        if hasattr(track, attr):
            try:
                setattr(track, attr, {})
            except Exception:
                pass

    # clear_cache if available
    if hasattr(track, "clear_cache"):
        try:
            track.clear_cache()
        except Exception:
            pass


# ---------------------------------------------------------------------
# ARENA CENTER ESTIMATION (no ghost filter, robust median)
# ---------------------------------------------------------------------

def estimate_arena_center(lf_list, node_name, max_frames=2000):
    """
    Estimate arena center using ALL nodes, not only the tracking node.
    Much more stable and prevents NaN center issues.
    """
    pts = []
    n_frames = len(lf_list)
    if n_frames == 0:
        return np.array([720.0, 720.0])

    step = max(1, n_frames // max_frames)

    for fi in range(0, n_frames, step):
        lf = lf_list[fi]
        for inst in lf.instances:
            for pt in inst.points:     # USE ALL POINTS
                if pt is not None and pt.x is not None and pt.y is not None:
                    pts.append([pt.x, pt.y])

    if len(pts) < 10:
        print("[WARN] Could not estimate center; using default (720,720).")
        return np.array([720.0, 720.0])

    pts = np.array(pts)
    return np.median(pts, axis=0)


# ---------------------------------------------------------------------
# MAIN TRAJECTORY BUILDER (NO GHOST FILTER)
# ---------------------------------------------------------------------

def build_two_track_trajectories(lf_list, node_name, tag_names, center, max_dtheta):
    """
    Build continuous trajectories for two logical tracks using
    angle-based Hungarian assignment with burn-in protection
    PLUS XY-distance gating to eliminate diagonal artifacts.

    Burn-in rule:
        - No output is written until BOTH tags have appeared and have
          valid last_pos and last_theta values.
    """

    def euclidean(p1, p2):
        return np.linalg.norm(p1 - p2)

    max_xy_jump = 80.0   # 🔥 absolute XY distance gating (pixels)

    assert len(tag_names) == 2, "Expected exactly two tags."

    n_frames = len(lf_list)
    trajectories = {
        tag: np.full((n_frames, 2), np.nan, float) for tag in tag_names
    }

    last_pos = {tag: None for tag in tag_names}
    last_theta = {tag: None for tag in tag_names}
    burn_in = {tag: False for tag in tag_names}

    def all_initialized():
        return all(burn_in.values())

    global CURRENT_FRAME
    missing_count = {tag: 0 for tag in tag_names}
    reset_gap = 200

    for f, lf in enumerate(lf_list):
        CURRENT_FRAME = f
        dbg_frame_header(f)

        # ----------------------------------------
        # COLLECT RAW DETECTIONS
        # ----------------------------------------
        coords = []
        for inst in lf.instances:
            node = inst[node_name]
            if node and node.x is not None and node.y is not None:
                coords.append(np.array([node.x, node.y], float))

        coords = np.array(coords, float) if len(coords) > 0 else []
        M = len(coords)

        # missing counter
        for tag in tag_names:
            if M == 0:
                missing_count[tag] += 1
            else:
                missing_count[tag] = 0

            if missing_count[tag] > reset_gap:
                dbg(f"[RESET] {tag} missing too long → resetting")
                last_pos[tag] = None
                last_theta[tag] = None
                burn_in[tag] = False
                missing_count[tag] = 0

        if M == 0:
            if all_initialized():
                for tag in tag_names:
                    if last_pos[tag] is not None:
                        trajectories[tag][f] = last_pos[tag]
            continue

        # ----------------------------------------
        # REMOVE NaN detections
        # ----------------------------------------
        valid = ~np.isnan(coords).any(axis=1)
        coords = coords[valid]
        if len(coords) == 0:
            if all_initialized():
                for tag in tag_names:
                    if last_pos[tag] is not None:
                        trajectories[tag][f] = last_pos[tag]
            continue

        # ----------------------------------------
        # COMPUTE DETECTION ANGLES
        # ----------------------------------------
        det_theta = np.array([angle_of_point(pt, center) for pt in coords])
        valid = ~np.isnan(det_theta)
        coords = coords[valid]
        det_theta = det_theta[valid]
        M = len(coords)

        dbg("Detections:", M)
        dbg("θ:", det_theta)
        dbg("Last θ:", last_theta)

        # ============================================================
        # BURN-IN MODE
        # ============================================================
        if not all_initialized():
            if M > 0:
                order = np.argsort(det_theta)
                for det_idx in order:
                    pt = coords[det_idx]
                    th = det_theta[det_idx]
                    uninit = [tag for tag in tag_names if not burn_in[tag]]
                    if not uninit:
                        break
                    tag = uninit[0]
                    burn_in[tag] = True
                    last_pos[tag] = pt
                    last_theta[tag] = th

            dbg(f"[BURN-IN] {burn_in}")
            continue  # IMPORTANT: write *nothing* during burn-in

        # ============================================================
        # FULL TRACKING
        # ============================================================

        # ----------------------------------------
        # CASE 1: ONE DETECTION
        # ----------------------------------------
        if M == 1:
            pt = coords[0]
            th = det_theta[0]

            # angle distances
            dists_theta = {
                tag: np.inf if last_theta[tag] is None
                else angular_distance(th, last_theta[tag])
                for tag in tag_names
            }
            best_tag = min(dists_theta, key=dists_theta.get)

            for tag in tag_names:
                if tag == best_tag:
                    # XY gating
                    if last_pos[tag] is not None:
                        d_xy = euclidean(pt, last_pos[tag])
                    else:
                        d_xy = 0

                    allow = (
                        (last_theta[tag] is None or dists_theta[tag] <= max_dtheta)
                        and (d_xy <= max_xy_jump)
                    )

                    dbg(f"[1-DET] {tag}: dθ={dists_theta[tag]:.3f}, dXY={d_xy:.1f}")

                    if allow:
                        trajectories[tag][f] = pt
                        last_pos[tag] = pt
                        last_theta[tag] = th
                    else:
                        # hold position
                        if last_pos[tag] is not None:
                            trajectories[tag][f] = last_pos[tag]
                else:
                    if last_pos[tag] is not None:
                        trajectories[tag][f] = last_pos[tag]

            continue

        # ----------------------------------------
        # CASE 2: ≥2 DETECTIONS → Hungarian
        # ----------------------------------------
        cost = np.zeros((2, M))
        for ti, tag in enumerate(tag_names):
            if last_theta[tag] is None:
                cost[ti, :] = 0
            else:
                cost[ti, :] = [
                    angular_distance(th, last_theta[tag])
                    for th in det_theta
                ]

        row_ind, col_ind = linear_sum_assignment(cost)
        assignment = {tag: None for tag in tag_names}

        for ti, tag in enumerate(tag_names):
            if ti in row_ind:
                idx = list(row_ind).index(ti)
                assignment[tag] = col_ind[idx]

        used = set()

        # ----------------------------------------
        # GATING ON ANGLE + XY
        # ----------------------------------------
        for tag in tag_names:
            idx = assignment[tag]
            if idx is None or last_theta[tag] is None or last_pos[tag] is None:
                continue

            new_th = det_theta[idx]
            dth = angular_distance(new_th, last_theta[tag])
            dxy = euclidean(coords[idx], last_pos[tag])

            accept = (dth <= max_dtheta) and (dxy <= max_xy_jump)
            dbg(f"[HUNGARIAN] {tag}: dθ={dth:.3f}, dXY={dxy:.1f}, accept={accept}")

            if not accept:
                assignment[tag] = None
            else:
                used.add(idx)

        # ----------------------------------------
        # FALLBACK ASSIGNMENT (angle + XY gating)
        # ----------------------------------------
        for tag in tag_names:
            if assignment[tag] is None:
                if last_theta[tag] is None or last_pos[tag] is None:
                    continue

                dists = []
                for i in range(M):
                    if i in used:
                        dists.append(np.inf)
                        continue
                    dth = angular_distance(det_theta[i], last_theta[tag])
                    dxy = euclidean(coords[i], last_pos[tag])
                    if dth <= max_dtheta and dxy <= max_xy_jump:
                        dists.append(dth)
                    else:
                        dists.append(np.inf)

                best = int(np.argmin(dists))
                if np.isfinite(dists[best]):
                    assignment[tag] = best
                    used.add(best)

        # ----------------------------------------
        # COMMIT FINAL POSITIONS
        # ----------------------------------------
        for tag in tag_names:
            idx = assignment[tag]
            if idx is None:
                if last_pos[tag] is not None:
                    trajectories[tag][f] = last_pos[tag]
                continue

            pt = coords[idx]
            th = det_theta[idx]
            trajectories[tag][f] = pt
            last_pos[tag] = pt
            last_theta[tag] = th

    return trajectories


# ---------------------------------------------------------------------
# WRITEBACK (USING REAL SLEAP TRACKS)
# ---------------------------------------------------------------------

def write_back_trajectories(labels, lf_list, node_name, tag_names, trajectories,
                            max_seg_jump=80.0):
    """
    Write trajectories back into SLEAP labels, but split each tag's track into
    separate segments when there is a very large XY jump between consecutive
    valid points. This prevents napari from drawing a big straight line
    across the arena.

    For each tag:
      - Start with its original track object.
      - If ||pos_t - pos_{t-1}|| > max_seg_jump, create a new track object
        (a shallow copy with a modified name) and continue writing to that.
    """
    skel = labels.skeleton

    # Find node index for the tag node
    node_idx = None
    for i, n in enumerate(skel.nodes):
        if n.name == node_name:
            node_idx = i
            break
    if node_idx is None:
        raise ValueError(f"Node {node_name} not found in skeleton.")

    # Base tracks for each tag
    base_tracks = {t.name: t for t in labels.tracks if t.name in tag_names}
    if len(base_tracks) != len(tag_names):
        raise ValueError(
            f"Could not find all tag tracks in labels.tracks. "
            f"Expected {tag_names}, found {list(base_tracks.keys())}"
        )

    # For each tag, keep a list of segment tracks: [segment0, segment1, ...]
    segment_tracks = {}
    segment_last_pos = {}
    segment_counts = {}

    for tag in tag_names:
        base = base_tracks[tag]
        segment_tracks[tag] = [base]  # first segment uses the original track
        segment_last_pos[tag] = None
        segment_counts[tag] = 1  # number of segments for this tag so far

    def get_current_track_for_tag(tag, pos):
        """
        For a given tag and new position pos, decide whether to keep the
        current segment track or start a new one (if the jump is too large).
        """
        last_pos = segment_last_pos[tag]

        if last_pos is not None and max_seg_jump is not None:
            dxy = float(np.linalg.norm(pos - last_pos))
            if dxy > max_seg_jump:
                # Large jump → start a new segment track
                base = base_tracks[tag]
                seg_idx = segment_counts[tag]
                # Shallow copy of the base track to keep same type
                new_track = copy.copy(base)
                new_track.name = f"{base.name}_seg{seg_idx}"
                labels.tracks.append(new_track)

                segment_tracks[tag].append(new_track)
                segment_counts[tag] += 1

                print(
                    f"[SEGMENT SPLIT] Tag {tag}: large jump dXY={dxy:.1f} "
                    f"→ new segment {new_track.name}"
                )

        return segment_tracks[tag][-1]

    # Clear instances for processed frames and write new ones
    for f_idx, lf in enumerate(lf_list):
        lf.instances = []  # clear originals in this processed chunk

        for tag in tag_names:
            pos = trajectories[tag][f_idx]

            # Skip frames where we don't have a valid position
            if pos is None or np.isnan(pos).any():
                continue

            # Decide which track segment to use
            track_for_this_frame = get_current_track_for_tag(tag, pos)

            # Build a point list for the full skeleton
            pts = [
                sleap.instance.Point(x=np.nan, y=np.nan, visible=False)
                for _ in skel.nodes
            ]

            # Replace only the tag node
            pts[node_idx] = sleap.instance.Point(
                float(pos[0]),
                float(pos[1]),
                visible=True,
            )

            inst = sleap.instance.Instance(
                points=pts,
                skeleton=skel,
                track=track_for_this_frame,
            )

            lf.instances.append(inst)

            # update last_pos for segment logic
            segment_last_pos[tag] = pos



# ---------------------------------------------------------------------
# MAIN PIPELINE
# ---------------------------------------------------------------------

def smooth_and_stabilize_labels(labels, node_name, max_dtheta, smooth_window, skip_frames, center):

    global CURRENT_FRAME
    CURRENT_FRAME = None

    try:
        lf_all = labels.labeled_frames
    except TypeError:
        lf_all = labels.labeled_frames()

    # remove all instances in the pre-skip frames so tracks
    # don't connect from the old raw tracking into the cleaned segment.
    if skip_frames > 0:
        for lf in lf_all[:skip_frames]:
            lf.instances = []
            if hasattr(lf, "instance_track_inds"):
                lf.instance_track_inds = []

        # fully wipe track internal caches BEFORE writing anything
        for t in labels.tracks:
            if hasattr(t, "_instance_idxs"):
                t._instance_idxs = {}
            if hasattr(t, "_last_instance"):
                t._last_instance = None
            if hasattr(t, "_last_frame"):
                t._last_frame = None
            if hasattr(t, "tracklet"):
                t.tracklet = None
            if hasattr(t, "points"):
                t.points = None
            if hasattr(t, "tracking_scores"):
                t.tracking_scores = None
            if hasattr(t, "clear_cache"):
                t.clear_cache()

    if skip_frames < 0:
        skip_frames = 0
    if skip_frames > len(lf_all):
        skip_frames = len(lf_all)

    lf_proc = lf_all[skip_frames:]
    n_proc = len(lf_proc)

    if n_proc == 0:
        print("[INFO] No frames to process; returning labels unchanged.")
        return labels

    tag_names = get_valid_track_names(labels)

    print(
        f"[INFO] max_dtheta={max_dtheta}, smooth_window={smooth_window}, "
        f"processing {n_proc} frames after skipping {skip_frames}"
    )

    # 1. Build raw trajectories
    print("  - Building raw trajectories...")
    traj = build_two_track_trajectories(
        lf_proc,
        node_name=node_name,
        tag_names=tag_names,
        center=center,
        max_dtheta=max_dtheta,
    )

    # 2. Interpolate, smooth, theta-clip, XY-clip
    print("  - Interpolating, smoothing, and clipping...")

    burn_in = 1   # initial frames of processed chunk to blank out
    max_interp_gap = 50  # frames
    max_xy_jump_post = 80.0  # 🔥 same as gating

    for tag in tag_names:
        dbg(f"Post-build: processing {tag}")
        traj_tag = traj[tag]

        # 🔥 Apply burn-in blackout to the *processed* chunk itself
        if burn_in > 0:
            n = min(burn_in, traj_tag.shape[0])
            traj_tag[:n, :] = np.nan

        # interpolate only across short gaps
        traj_tag = interpolate_nan(traj_tag, max_gap=max_interp_gap)

        # smooth
        traj_tag = smooth_traj(traj_tag, smooth_window)

        # clip by angle
        traj_tag = clip_jumps_theta(traj_tag, center, max_dtheta)

        # 🔥 FINAL SAFETY: kill any huge XY jumps that survive everything else
        traj_tag = clip_jumps_xy(traj_tag, max_xy_jump_post)

        traj[tag] = traj_tag

    # 3. Write back
    clear_track_cache(labels)
    print("  - Writing trajectories back into labels...")
    write_back_trajectories(
        labels,
        lf_proc,
        node_name,
        tag_names,
        traj,
        max_seg_jump=80.0,   # or tweak this threshold if needed
    )


    # 4. Reset track objects to remove old history
    for t in labels.tracks:
        full_reset_track_object(t)

    return labels


# ---------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------

def main():
    global DEBUG, DEBUG_FRAME, DEBUG_RANGE

    parser = argparse.ArgumentParser(
        description="Stabilize and smooth two-tag trajectories in a circular arena."
    )

    parser.add_argument("--input", required=True, help="Input .slp file")
    parser.add_argument("--output", required=True, help="Output .slp file")
    parser.add_argument("--node", required=True, help="Node name to process (e.g., 'tag')")
    parser.add_argument("--smooth-window", type=int, default=3,
                        help="Moving average window size")
    parser.add_argument("--max-dtheta", type=float, default=0.20,
                        help="Max allowed angular change per frame (radians)")
    parser.add_argument("--skip-frames", type=int, default=200,
                        help="Number of initial frames to skip before stabilization")
    parser.add_argument("--skip-hide", action="store_true",
                        help="Skip hiding all other nodes")
    parser.add_argument("--debug", action="store_true",
                        help="Enable verbose debug output")
    parser.add_argument("--debug-frame", type=int, default=None,
                        help="Frame to focus debug logging on")
    parser.add_argument("--debug-range", type=int, default=0,
                        help="± range of frames to show around debug-frame")
    args = parser.parse_args()

    DEBUG = args.debug
    DEBUG_FRAME = args.debug_frame
    DEBUG_RANGE = args.debug_range

    print(f"Loading: {args.input}")
    labels = sleap.load_file(args.input)

    # 1. Extract labeled frames for center estimate
    try:
        lf_all = labels.labeled_frames
    except TypeError:
        lf_all = labels.labeled_frames()

    lf_proc = lf_all[args.skip_frames:]

    # 2. Estimate arena center BEFORE hiding nodes
    center = estimate_arena_center(lf_proc, args.node)
    print(f"[ARENA] center={center}")

    # 3. Now hide nodes
    if not args.skip_hide:
        print(f"Hiding all nodes except: {args.node}")
        labels = hide_all_but_node(labels, args.node)
    else:
        print("Skipping hide step.")

    # 4. Stabilization pipeline
    print(
        f"Stabilizing and smoothing trajectories "
        f"(max_dtheta={args.max_dtheta}, smooth_window={args.smooth_window})"
    )

    labels = smooth_and_stabilize_labels(
        labels,
        node_name=args.node,
        max_dtheta=args.max_dtheta,
        smooth_window=args.smooth_window,
        skip_frames=args.skip_frames,
        center=center,
    )

    debug_dump_selected_frames(
        labels,
        args.node,
        get_valid_track_names(labels),
        args.skip_frames
    )

    print(f"Saving filtered file → {args.output}")
    labels.save(args.output)
    print("Done!")


if __name__ == "__main__":
    main()
