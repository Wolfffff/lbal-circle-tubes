# Comparative pipeline

Tracks the circle-tube videos of every species and writes the same per-video files as
`../data/processed_data/` (interactions, `*_hth_bouts.csv` / `*_htb_bouts.csv`, `*_vels.csv`;
bees `ArUcoTag#black` / `ArUcoTag#purple`), so `../scripts/R/` can run on all of them.

```
video ─ sleap-nn pose + tracking ─ tag brightness ─ NAPS identity ─ smoothing ─ interactions / bouts / velocities
```

## Run (Della)

```bash
cd comparative_pipeline                      # on /scratch
bash envs/setup.sh                           # pinned envs: .venv-sleap, .venv-naps, .venv-smk (needs uv)
SMK=".venv-smk/bin/snakemake --profile profiles/della"   # SLURM account: profiles/della/config.yaml
$SMK evaluate      # train v3 + v3b, held-out accuracy   -> results/<model>/eval.tsv
$SMK               # all videos of all datasets         -> results/v3b/<dataset>/...
$SMK identbench    # held-out identity test             -> results/v3b/identbench/summary.tsv
$SMK loso          # leave-one-species-out              -> results/v3/loso.tsv
```

Datasets, video folders and every parameter are in `config/config.yaml`; corrected frames are in
`labels/`. Videos run in batches (~50 SLURM jobs for everything) and reruns only redo what's missing.

Per video, in `results/v3b/<dataset>/`: `tracked/` (pose), `naps/` (identity), `filtered/`
(`.slp` + `.analysis.h5` trajectories), `behavior/` (the files above + `summary.json`);
per dataset: `behavior.tsv`, `qc.tsv`.

## Changes from the original processing

| step | before | now |
|---|---|---|
| pose | SLEAP 1.2.9 per-species fine-tunes | one sleap-nn model (`v3b`) on base LBAL + all species' corrected frames, thorax-anchored, rotation/scale/brightness augmentation |
| tag colour | `classifier.pkl` (`scripts/python/naps/`) | brightness at the tag node, per-video threshold (`naps-color-decodes`) |
| identity | `color_track.py` | NAPS joint engine, 2-bee closed set ([Wolfffff/naps](https://github.com/Wolfffff/naps) `wolf-naps-colortag`) |
| trajectories | `filter_nodes.py` | `scripts/smooth_tracks.py` |
| behaviour | `naps_interactions.py`, `analyze_network.py`, `culledvelocities.py` | `scripts/behavior.py`, same interaction model (`configs/interactions.yaml`) and 20-frame bout gap |

Why:

1. **Fine-tunes never fine-tuned.** SLEAP 1.2.9 ignores `--base_checkpoint`; the per-species
   models trained from scratch on 54 frames and returned no keypoints.
2. **Colour classifier.** The videos are grayscale: purple = bright disc, black ≈ invisible
   (`figures/tag_crops.png`). `classifier.pkl` calls both bees "black" in ~70 % of labelled LBAL
   pairs. Its training (`scripts/python/select_color_tags.ipynb`) never resets the mask between
   clicks (near-duplicate rows, leaked test split), uses different pixels than inference
   (`color_tag.py`), and was trained on ~640 px frames but applied to 1440 px video.
3. **`filter_nodes.py` discards identity.** It re-labels the bees by angle around the arena
   (arbitrary black/purple per video) and, on a test video, put both names on one bee.
   Identity-dependent results from its outputs should be redone.
4. **Behaviour files.** Bouts are now split per (origin, destination) pair; velocity is the thorax
   of both bees, columns black then purple.

## Results

Held-out frames (`results/v3b/eval.tsv`; median px error, 1440–1832 px frames):

| | base (LBAL) | aurata | melanderi | pura | zephyrus |
|---|---|---|---|---|---|
| bees found | 99.1 % | 100 % | 100 % | 100 % | 100 % |
| keypoints | 2.8 | 4.6 | 3.5 | 3.6 | 2.4 |
| tag point | 1.9 | 2.8 | 1.7 | 1.4 | 1.9 |

- **Identity** (v3 tracks, 20 % of tag readings withheld): 98.3 % (aurata), 100 % (pura), 99.4 % (zephyrus)
  correct; 96–100 % of bee-frames labelled.
- **Unlabelled species** (leave-one-species-out): keypoint error roughly doubles (e.g. aurata
  4.7 → 8 px, melanderi 3.7 → 13 px); 60 corrected frames bring it back.

![zephyrus](figures/predictions_zephyrus.jpg)

## Open issues

- **melanderi**: both tags read bright, so it is tracked without identity (`identity: false`).
  Which colours were used?
- **mm/px**: the motion fraction uses 0.0946 mm/px (LBAL rig); set `mm_per_px` per dataset.
  `scripts/R/utils.R` counts motion from one bee's column only.
- **oenotherae** has no corrected frames; ~60 would help most. The 2026-06-22 session is darker
  than the rest (`figures/dark_oenotherae_session.jpg`); `v3b` handles it, `v3` does not.
- `local_26` mixes aurata / pura / zephyrus (species per video: `metadata/local_26_videos.tsv`).
