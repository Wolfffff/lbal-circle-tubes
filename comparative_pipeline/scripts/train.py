"""Train (or fine-tune) a SLEAP top-down model with sleap-nn.

Model architecture/preprocessing/augmentation come from a legacy SLEAP
training_config.json (the base LBAL model's recipe). Labels can be several
.slp files (e.g. base + every species); they must share one skeleton.

Replaces `sleap-train --base_checkpoint ...` in SLEAP 1.2.9, which silently
ignored that flag, so the old "fine-tunes" were trained from scratch on 54
frames and the centered-instance models collapsed to blank confidence maps.

Examples
  # one model on all labels, from scratch
  python train.py --config configs/centroid_legacy.json \
      --labels base/base_labels.slp aurata/aurata_labels.slp ... \
      --out models --run-name all_species.centroid
  # fine-tune from a legacy model's weights
  python train.py --config <model>/training_config.json --init-weights <model>/best_model.h5 ...
"""

import argparse
import json
from pathlib import Path

import sleap_io as sio
from omegaconf import OmegaConf
from sleap_nn.config.training_job_config import TrainingJobConfig
from sleap_nn.train import run_training


def set_backbone(cfg, a):
    """Apply --backbone / --filters / --max-stride / --input-scale to a loaded config."""
    from sleap_nn.config.model_config import ConvNextConfig, SwinTConfig

    bb = cfg.model_config.backbone_config
    old = bb.unet
    if a.backbone in ("convnext", "swint"):
        out_stride = old.output_stride if old is not None else 1
        if a.backbone == "convnext":
            weights = None if a.no_pretrained else f"ConvNeXt_{a.backbone_size.title()}_Weights"
            bb.convnext = ConvNextConfig(model_type=a.backbone_size, pre_trained_weights=weights,
                                         output_stride=out_stride)
        else:
            weights = None if a.no_pretrained else f"Swin_{a.backbone_size[0].upper()}_Weights"
            bb.swint = SwinTConfig(model_type=a.backbone_size, pre_trained_weights=weights,
                                   output_stride=out_stride)
        bb.unet = None
        print(f"backbone -> {a.backbone} {a.backbone_size} (weights {weights}), "
              f"output_stride {out_stride}")
    elif old is not None:
        if a.filters:
            old.filters = a.filters
        if a.max_stride:
            old.max_stride = a.max_stride
        print(f"backbone: unet filters {old.filters}, max_stride {old.max_stride}, "
              f"output_stride {old.output_stride}")
    if a.input_scale:
        cfg.data_config.preprocessing.scale = a.input_scale
        print(f"input scale -> {a.input_scale}")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--config", required=True, help="legacy SLEAP training_config.json")
    p.add_argument("--labels", nargs="+", required=True,
                   help="training .slp files; repeat a path to oversample it")
    p.add_argument("--val-labels", nargs="+", default=None,
                   help="fixed validation .slp files (default: random --val-fraction split)")
    p.add_argument("--out", required=True)
    p.add_argument("--run-name", required=True)
    p.add_argument("--init-weights", default=None,
                   help="legacy best_model.h5 or sleap-nn .ckpt to start from")
    p.add_argument("--lr", type=float, default=1e-4)
    p.add_argument("--epochs", type=int, default=200)
    p.add_argument("--batch-size", type=int, default=4)
    p.add_argument("--steps-per-epoch", type=int, default=200)
    p.add_argument("--val-fraction", type=float, default=0.1)
    p.add_argument("--patience", type=int, default=20)
    p.add_argument("--cache", choices=["none", "memory", "disk"], default="memory")
    p.add_argument("--workers", type=int, default=4)
    # recipe overrides (default: keep the legacy config's values)
    p.add_argument("--anchor", default=None,
                   help="anchor node for centroid / centered-instance crops (e.g. thorax)")
    p.add_argument("--crop-size", type=int, default=None, help="centered-instance crop (multiple of 32)")
    p.add_argument("--rotation", type=float, default=None, help="random rotation +/- degrees")
    p.add_argument("--scale-range", type=float, nargs=2, default=None, help="random scale min max")
    # backbone overrides (default: the legacy config's UNet)
    p.add_argument("--backbone", choices=["unet", "convnext", "swint"], default=None,
                   help="replace the backbone (convnext/swint start from ImageNet weights)")
    p.add_argument("--backbone-size", default="tiny", help="convnext/swint size: tiny|small|base")
    p.add_argument("--no-pretrained", action="store_true", help="convnext/swint from scratch")
    p.add_argument("--filters", type=int, default=None, help="UNet base filters")
    p.add_argument("--max-stride", type=int, default=None, help="UNet max stride (16 or 32)")
    p.add_argument("--input-scale", type=float, default=None, help="input image scale")
    # intensity augmentation (the base centroid recipe has none, which breaks on dark videos)
    p.add_argument("--brightness", type=float, nargs=2, default=None,
                   help="random brightness factor range, e.g. 0.5 1.5")
    p.add_argument("--contrast", type=float, nargs=2, default=None,
                   help="random contrast factor range, e.g. 0.6 1.5")
    p.add_argument("--intensity-p", type=float, default=0.75,
                   help="probability of each intensity augmentation")
    a = p.parse_args()

    cfg = TrainingJobConfig.load_sleap_config(a.config)
    set_backbone(cfg, a)

    heads = cfg.model_config.head_configs
    for name in ("centroid", "centered_instance"):
        h = heads.get(name)
        if h is not None and a.anchor:
            h.confmaps.anchor_part = a.anchor
            print(f"{name}: anchor_part -> {a.anchor}")
    if a.crop_size and heads.get("centered_instance") is not None:
        cfg.data_config.preprocessing.crop_size = a.crop_size
        print(f"crop_size -> {a.crop_size}")
    g = cfg.data_config.augmentation_config.geometric
    if a.rotation is not None:
        g.rotation_min, g.rotation_max = -a.rotation, a.rotation
    if a.scale_range is not None:
        g.scale_min, g.scale_max = a.scale_range
    print(f"aug: rotation {g.rotation_min}..{g.rotation_max}, scale {g.scale_min}..{g.scale_max}")
    it = cfg.data_config.augmentation_config.intensity
    if a.brightness is not None:
        it.brightness_min, it.brightness_max = a.brightness
        it.brightness_p = a.intensity_p
    if a.contrast is not None:
        it.contrast_min, it.contrast_max = a.contrast
        it.contrast_p = a.intensity_p
    print(f"aug: brightness {it.brightness_min}..{it.brightness_max} (p {it.brightness_p}), "
          f"contrast {it.contrast_min}..{it.contrast_max} (p {it.contrast_p})")

    # ---- data
    names = None
    total = 0
    for lp in list(dict.fromkeys(a.labels + (a.val_labels or []))):
        L = sio.load_slp(lp, open_videos=False)
        lfs = [lf for lf in L.labeled_frames if lf.user_instances]
        n_inst = sum(len(lf.user_instances) for lf in lfs)
        total += len(lfs)
        print(f"labels {lp}: {len(lfs)} frames, {n_inst} user instances")
        if names is None:
            names = L.skeleton.node_names
        elif L.skeleton.node_names != names:
            raise SystemExit(f"skeleton mismatch in {lp}")
    print(f"total labeled frames: {total}")

    d = cfg.data_config
    d.train_labels_path = [str(Path(x).resolve()) for x in a.labels]
    if a.val_labels:
        d.val_labels_path = [str(Path(x).resolve()) for x in a.val_labels]
    else:
        d.val_labels_path = None
        d.validation_fraction = a.val_fraction
    d.user_instances_only = True
    fw = {"none": "torch_dataset", "memory": "torch_dataset_cache_img_memory",
          "disk": "torch_dataset_cache_img_disk"}[a.cache]
    d.data_pipeline_fw = fw
    if a.cache == "disk":
        d.cache_img_path = str(Path(a.out).resolve() / f"{a.run_name}_imgcache")

    # ---- weights
    if a.init_weights:
        w = Path(a.init_weights)
        if not w.exists():
            raise SystemExit(f"missing {w}")
        cfg.model_config.pretrained_backbone_weights = str(w)
        cfg.model_config.pretrained_head_weights = str(w)

    # ---- optimization
    tc = cfg.trainer_config
    tc.optimizer.lr = a.lr
    tc.max_epochs = a.epochs
    tc.train_steps_per_epoch = None
    tc.min_train_steps_per_epoch = a.steps_per_epoch
    tc.train_data_loader.batch_size = a.batch_size
    tc.val_data_loader.batch_size = a.batch_size
    # video backends can't be pickled -> workers only with an image cache
    nw = a.workers if a.cache != "none" else 0
    tc.train_data_loader.num_workers = nw
    tc.val_data_loader.num_workers = nw
    tc.early_stopping.patience = a.patience
    tc.save_ckpt = True
    tc.ckpt_dir = str(Path(a.out).resolve())
    tc.run_name = a.run_name
    tc.visualize_preds_during_training = False
    tc.use_wandb = False

    out = Path(a.out) / a.run_name
    out.mkdir(parents=True, exist_ok=True)
    (out / "train_args.json").write_text(json.dumps(vars(a), indent=2))
    print(OmegaConf.to_yaml(cfg.model_config))
    try:
        run_training(cfg)
    except Exception as e:
        # sleap-nn's post-training eval assumes len(train files) == len(val files)
        # (it zips them when saving labels_gt.*), which oversampling breaks. Training and
        # the checkpoint are already done by then; our own evaluate step does the eval.
        if (out / "best.ckpt").exists():
            print(f"WARNING: post-training eval failed ({type(e).__name__}: {e}); "
                  f"keeping {out / 'best.ckpt'}", flush=True)
        else:
            raise


if __name__ == "__main__":
    main()
