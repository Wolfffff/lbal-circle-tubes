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
$SMK evaluate      # train v3, v3b, v4_convnext; held-out accuracy -> results/<model>/eval.tsv
$SMK               # all videos of all datasets         -> results/v4_convnext/<dataset>/...
$SMK identbench    # held-out identity test             -> results/v4_convnext/identbench/summary.tsv
$SMK loso          # leave-one-species-out              -> results/<model>/loso.tsv
```

Datasets, video folders and every parameter are in `config/config.yaml`; corrected frames are in
`labels/`. Videos run 4 per GPU job on Della's MIG slices (`batch:` in the config; ~80 GPU + ~25 CPU jobs
for all 275 videos) and reruns only redo what's missing.

Per video, in `results/v4_convnext/<dataset>/`: `tracked/` (pose), `naps/` (identity), `filtered/`
(`.slp` + `.analysis.h5` trajectories), `behavior/` (the files above + `summary.json`);
per dataset: `behavior.tsv` (speeds in mm/s via the rig's `mm_per_px`; `immobile_bees` flag), `qc.tsv`.
`scripts/showcase.py` renders example clips (smoothed with [`movement`](https://movement.neuroinformatics.dev)).

## Changes from the original processing

| step | before | now |
|---|---|---|
| pose | SLEAP 1.2.9 per-species fine-tunes | one sleap-nn model (`v4_convnext`: UNet centroid + ImageNet ConvNeXt-Tiny pose) on base LBAL + all species' corrected frames, thorax-anchored, rotation/scale/brightness augmentation |
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

Held-out frames (`results/v4_convnext/eval.tsv`; px error, 1440–1832 px frames):

| | base (LBAL) | aurata | melanderi | pura | zephyrus |
|---|---|---|---|---|---|
| bees found | 99.1 % | 100 % | 100 % | 100 % | 100 % |
| keypoints, median | 2.5 | 3.8 | 2.9 | 2.5 | 2.6 |
| keypoints, 90th pct | 5.6 | 20.0 | 5.8 | 5.2 | 3.7 |
| tag point, median | 1.7 | 2.7 | 1.7 | 1.4 | 1.4 |

v3 / v3b (UNet only): median 2.8 / 4.6 / 3.5 / 3.6 / 2.4, 90th pct 8.2 / 15.6 / 8.5 / 8.2 / 3.6.

- **Identity** (20 % of tag readings withheld, one test video per species): 98.2 % (aurata), 100 % (pura),
  95.7 % (zephyrus; 99.4 % with v3) correct; 93–97 % of bee-frames labelled.
- Every video gets both identities with v4 (v3 failed on 3: the dark oenotherae session and a
  zephyrus video where the arena label was detected as a bee).
- **Unlabelled species** (leave-one-species-out): median keypoint error aurata 3.8 → 6.0 px,
  melanderi 2.9 → 11.5 px (v3: 4.7 → 8.1, 3.7 → 13.5); 60 corrected frames bring it back.

![zephyrus](figures/predictions_zephyrus.jpg)

## Open issues

- **melanderi**: purple + black, but from different paper: the black tag reads mid-grey (tag
  brightness ~80–105 vs ~20–50 elsewhere), so the two tags separate cleanly in only ~3 of 10 videos.
  Tracked without identity (`identity: false`).
- **mm/px** per rig (`datasets.<ds>.mm_per_px`): aurata / pura / zephyrus / local_26 103.6 px/cm,
  melanderi 77.4, oenotherae 97.5, LBAL 105.7 (0.0946 mm/px). local_26 assumed to be the aurata/pura/zephyrus rig.
  `scripts/R/utils.R` counts motion from one bee's column only.
- **oenotherae** has no corrected frames; ~60 would help most. The 2026-06-22 session is darker
  than the rest (`figures/dark_oenotherae_session.jpg`); `v3b`/`v4_convnext` handle it, `v3` does not.
- `local_26` mixes aurata / pura / zephyrus (species per video: `metadata/local_26_videos.tsv`).
- **Possibly immobile bees** (`immobile_bees` in `behavior.tsv`: moving in < 5 % of frames; median
  bee 46 %): both bees in aurata `Camera1_20250815_144929_TR` and pura `Camera2_20250815_144932_TR`;
  one bee in aurata `Camera1_20250815_144929_TL`, zephyrus `20250704_1517_Camera0_TL`,
  `Camera1_20250805_120715_BL`/`BR`, `Camera1_20250814_112009_TR`, local_26 `20260603_1317_Camera1_TL`,
  `20260604_1239_Camera2_BR`. Check before analysis.
- Six tracked local_26 crops are not in the 2026 Drive folders: `20260504_1244_Camera1_TR`,
  `20260519_1225_Camera1_TL`, `20260526_1233_Camera1_BL`, `20260528_1300_Camera1_TL`/`TR`,
  `20260604_1239_Camera1_BR`. Exclude?
