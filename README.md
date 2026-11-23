<img src="static/kocher_lab_logo.png" alt="Kocher Lab logo" width="50%">

# Circle tube behavior

Two sweat bees are placed in a circle tube arena and filmed from above. Each bee has a paint dot
on its thorax, one purple and one black. We measure how much each bee moves and how often the two
meet head to head or head to body. This repo holds the *Lasioglossum baleicum* (LBAL, 2023)
analysis, including the R stats and paper figures. The current tracking code for all species
(baleicum, aurata, pura, zephyrus, oenotherae, melanderi) is on the
[`comparative-pipeline`](../../tree/comparative-pipeline/comparative_pipeline) branch.

## Pipeline

```
label frames (SLEAP GUI) → train sleap-nn → inference → OC-SORT tracking → tag brightness
  → NAPS identity → smoothing → speed + interactions → stats
```

| Step | What | Where |
|---|---|---|
| Label | Correct ~60 frames per species in the SLEAP GUI | Local |
| Train | One sleap-nn top-down model pooled across species (`v4_convnext`: UNet centroid + ConvNeXt-Tiny pose) | Della GPU |
| Inference + tracking | sleap-nn predict, at most 2 bees, OC-SORT tracker | Della GPU (MIG slices) |
| Tag brightness | Brightness at the `tag` node in every frame, with a per-video threshold (`naps-color-decodes`) | Della CPU |
| Identity | NAPS joint engine: 2-bee closed set, carries identity along each track | Della CPU |
| Smoothing | Remove spikes, fill gaps of 10 frames or fewer, 5-frame running median | Della CPU |
| Speed + interactions | Per-bee speed (mm/s), fraction of time moving, head-to-head / head-to-body bouts | Della CPU |
| Stats | `scripts/R/` | Local |

Steps 2–7 run as one Snakemake workflow (`comparative_pipeline/` on that branch).

## Versions

- **sleap-nn 0.3.5** (PyTorch). This replaces SLEAP 1.2.x.
- **NAPS**: [Wolfffff/naps](https://github.com/Wolfffff/naps), branch `wolf-naps-colortag`, commit
  `c5ed68f`. This branch adds brightness tag reading (`naps-color-decodes`) and the 2-bee joint
  identity engine (`--closed-set 0 1 --colony-size 2`).

## Recent changes

- **sleap-nn replaces SLEAP 1.2.x.** SLEAP 1.2.9 silently ignored `--base_checkpoint`, so the old
  per-species "fine-tunes" were trained from scratch on about 54 frames.
- **One pooled model.** It is trained on all species with a ConvNeXt backbone, which gives the
  lowest error on held-out frames and on species the model hasn't seen.
- **Identity from tag brightness.** The videos are grayscale, so this replaces the colour
  classifier (`classifier.pkl`). That classifier called both bees "black" in about 70% of
  frames.
- **Per-rig mm/px values** come from Michelle. Speeds are in mm/s.
- **Don't trust old outputs for identity.** `filter_nodes.py` re-assigned the IDs by angle and
  could put both IDs on one bee. Don't use the old `classifier.pkl` / `filter_nodes.py` outputs
  for identity-dependent results.

## Circle tags are not ArUco tags

- **ArUco** (Bombus): a printed square code with many IDs. NAPS reads the code, so it works with
  many bees per arena.
- **Circle tags** (sweat bees): a paint dot that only shows up as bright or dark in grayscale.
  This works for 2 bees per tube if the two colours differ in brightness. melanderi has no
  identity because both of its colours read bright.
- Output track names still say `ArUcoTag#black` / `ArUcoTag#purple`. That is only the NAPS
  naming; these are circle tags.

## Where to run what

| Machine | Use |
|---|---|
| Local | Labelling, checking tracks by eye, R stats and figures |
| Della | Training, inference, NAPS, the Snakemake pipeline (`/scratch/gpfs/SKOCHER/...`; scratch is not backed up) |
| Argo | Long-term storage for raw video and intermediates |
| Berkeley machines | To confirm with Michelle |

## Data tracking

Everything is keyed on the video name (e.g. `20230619_113305_TR`).

- **`videos.tsv`** (proposed): one row per video. Columns: `video, species, year, date, rig,
  quadrant, fps, mm_per_px, raw_path, usable, reason, pipeline_version, identity_coverage`.
- **Bee sheet** (like `data/LBAL_Master_Data_Sheet.csv`): one row per bee, with tag colour,
  partner, caste, nest and morphology. `Remove? = Yes` drops a bee in `scripts/R/utils.R`.

## Repo layout

```
data/LBAL_Master_Data_Sheet.csv   one row per bee (LBAL)
data/video_df.csv                 video → caste contrast (built by scripts/R/utils.R)
data/manifest.csv                 stray Bombus file, ignore
data/processed_data/              LBAL per-video velocities, interactions, bouts (+ old SLURM scripts)
scripts/R/                        stats and figures (run from repo root)
scripts/python/naps/              old colour classifier (superseded)
figures/                          paper figures
```

## Contact

Michelle White and Scott Wolf, Kocher Lab, Princeton.
